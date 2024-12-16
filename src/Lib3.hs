{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
    ) where

import Control.Concurrent ( Chan )
import Control.Concurrent.STM (STM, TVar)
import qualified Lib2
import Data.List (isPrefixOf)  -- Import isPrefixOf4
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Concurrent (forkIO, Chan, newChan, writeChan, readChan)
import System.IO (withFile, IOMode(ReadMode, WriteMode), hGetContents, hPutStr)
import Control.Monad (forever)

filename :: String
filename = "state.txt"

data StorageOp = Save String (Chan ()) | Load (Chan String)
data Statements = Batch [Lib2.Command]
               | Single Lib2.Command
               deriving (Show, Eq)

data Command = StatementCommand Statements
             | LoadCommand
             | SaveCommand
             deriving (Show, Eq)

-- Parses user's input
parseCommand :: String -> Either String (Command, String)
parseCommand input =
    case words input of
        ("load":_) -> Right (LoadCommand, "")
        ("save":_) -> Right (SaveCommand, "")
        _ -> case parseStatements input of
                Right (statements, rest) -> Right (StatementCommand statements, rest)
                Left err -> Left err

-- Parses Statement: either a batch or a single command
parseStatements :: String -> Either String (Statements, String)
parseStatements input
    | "BEGIN" `elem` words input = parseBatch input
    | otherwise = parseSingle input

-- Parse batch of commands wrapped in BEGIN ... END
parseBatch :: String -> Either String (Statements, String)
parseBatch input = 
    case parseString "BEGIN" input of
        Right (_, rest1) -> 
            let commands = parseMultipleCommands (dropWhile (`elem` " \n") rest1)  -- Skip leading whitespace and newlines
            in case commands of
                Right (cmds, rest2) -> 
                    case parseString "END" (dropWhile (`elem` " \n") rest2) of
                        Right (_, remaining) ->
                            if all (`elem` " \n") remaining  -- Ensure only whitespace/newlines remain
                                then Right (Batch cmds, "")
                                else Left $ "Unexpected input after END: " ++ remaining
                        Left err -> Left $ "Missing END: " ++ err
                Left err -> Left err
        Left err -> Left err

-- Parse a single command
parseSingle :: String -> Either String (Statements, String)
parseSingle input = 
    case Lib2.parseQuery input of
        Right query -> Right (Single query, "")  -- Return the command without leftover input
        Left err -> Left err

-- Parse multiple commands separated by semicolons
parseMultipleCommands :: String -> Either String ([Lib2.Command], String)
parseMultipleCommands input = go input []
  where
    go :: String -> [Lib2.Command] -> Either String ([Lib2.Command], String)
    go [] acc = Right (reverse acc, "")
    go str acc =
        let (cmdStr, rest) = break (== ';') str  -- Split at semicolon
        in case Lib2.parseQuery (trim cmdStr) of
            Right command ->
                let rest' = dropWhile (`elem` "; \n") rest  -- Skip semicolon, spaces, and newlines
                in if "END" `isPrefixOf` rest'
                    then Right (reverse (command : acc), rest')
                    else go rest' (command : acc)
            Left err -> Left err

-- Utility function to parse a specific keyword
parseString :: String -> String -> Either String (String, String)
parseString keyword input =
    let trimmed = dropWhile (== ' ') input
    in if take (length keyword) trimmed == keyword
       then Right (keyword, drop (length keyword) trimmed)
       else Left $ "Expected " ++ keyword ++ ", found " ++ take (length keyword) trimmed
-- STM-based State Transition Function
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition stateVar command ioChan = case command of
    StatementCommand (Single query) -> executeSingle stateVar query
    StatementCommand (Batch queries) -> executeBatch stateVar queries
    LoadCommand -> executeLoad stateVar ioChan
    SaveCommand -> executeSave stateVar ioChan

-- Execute a single command atomically
executeSingle :: TVar Lib2.State -> Lib2.Command -> IO (Either String (Maybe String))
executeSingle stateVar query = atomically $ do
    currentState <- readTVar stateVar
    case Lib2.stateTransition currentState query of
        Right (output, newState) -> do
            writeTVar stateVar newState  -- Update state atomically
            return $ Right (Just (unlines output))
        Left err -> return $ Left err  -- No update if command fails

-- Execute a batch of commands atomically
executeBatch :: TVar Lib2.State -> [Lib2.Command] -> IO (Either String (Maybe String))
executeBatch stateVar queries = atomically $ do
    currentState <- readTVar stateVar
    let applyBatch state [] = Right ([], state)
        applyBatch state (q:qs) =
            case Lib2.stateTransition state q of
                Right (output, newState) -> 
                    case applyBatch newState qs of
                        Right (outputs, finalState) -> Right (output ++ outputs, finalState)
                        Left err -> Left err
                Left err -> Left err

    case applyBatch currentState queries of
        Right (outputs, newState) -> do
            writeTVar stateVar newState
            return $ Right (Just (unlines outputs))
        Left err -> return $ Left err

executeSave :: TVar Lib2.State -> Chan StorageOp -> IO (Either String (Maybe String))
executeSave stateVar ioChan = do
    currentState <- atomically $ readTVar stateVar
    let statements = marshallState currentState
        rendered = renderStatements statements
    sync <- newChan
    writeChan ioChan (Save rendered sync)
    readChan sync
    return $ Right (Just "State saved successfully!")

-- Load the state from a file
executeLoad :: TVar Lib2.State -> Chan StorageOp -> IO (Either String (Maybe String))
executeLoad stateVar ioChan = do
    sync <- newChan
    writeChan ioChan (Load sync)
    result <- readChan sync
    let linesOfCommands = lines result
    case mapM parseCommandLine linesOfCommands of
        Right commands -> do
            -- Apply all commands sequentially
            _ <- applyCommands stateVar commands
            return $ Right (Just "State loaded successfully!")
        Left err -> return $ Left $ "Failed to load state: " ++ err

-- Parse a single command line
parseCommandLine :: String -> Either String Lib2.Command
parseCommandLine input =
    case Lib2.parseQuery input of
        Right command -> Right command
        Left err -> Left $ "Error parsing command: " ++ err

-- Apply multiple commands atomically
applyCommands :: TVar Lib2.State -> [Lib2.Command] -> IO (Either String ())
applyCommands stateVar commands = atomically $ do
    currentState <- readTVar stateVar
    let result = foldl applyCommand (Right currentState) commands
    case result of
        Right newState -> do
            writeTVar stateVar newState
            return $ Right ()
        Left err -> return $ Left err
  where
    applyCommand (Left err) _ = Left err
    applyCommand (Right state) command =
        case Lib2.stateTransition state command of
            Right (_, newState) -> Right newState
            Left err -> Left err

-- File handling thread
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = forever $ do
    let handleSave contents = withFile filename WriteMode (\h -> hPutStr h contents)
        handleLoad = withFile filename ReadMode (\h -> do
            contents <- hGetContents h
            length contents `seq` return contents)  -- Force full evaluation of contents
    op <- readChan chan
    case op of
        Save contents sync -> do
            handleSave contents
            writeChan sync ()
        Load sync -> do
            result <- handleLoad
            writeChan sync result


-- Converts program's state into Statements
marshallState :: Lib2.State -> Statements
marshallState state = Batch (serializeState state)  -- Implement `serializeState`

-- Renders Statements into a String
renderStatements :: Statements -> String
renderStatements (Batch commands) = unlines (map renderCommand commands)
renderStatements (Single command) = renderCommand command

serializeState :: Lib2.State -> [Lib2.Command]
serializeState (Lib2.State cars services) = 
    -- Serialize cars into AddCar commands
    map (\car -> Lib2.AddCar (Lib2.carPlate car) (Lib2.carMake car) (Lib2.carModel car) (Lib2.carYear car)) cars
    ++
    -- Serialize services into ServiceCar commands
    map (\service -> Lib2.ServiceCar (Lib2.serviceCarPlate service) (Lib2.serviceTypes service) (Lib2.serviceDate service)) services

deserializeState :: [Lib2.Command] -> Lib2.State
deserializeState commands =
    Lib2.State cars services
  where
    cars = [ Lib2.Car plate make model year
           | Lib2.AddCar plate make model year <- commands ]
    services = [ Lib2.Service plate types date
               | Lib2.ServiceCar plate types date <- commands ]

renderCommand :: Lib2.Command -> String
renderCommand (Lib2.AddCar plate make model year) =
    "add car " ++ plate ++ " " ++ make ++ " " ++ model ++ " " ++ show year
renderCommand (Lib2.RemoveCar plate) =
    "remove car " ++ plate
renderCommand (Lib2.ServiceCar plate serviceTypes date) =
    "service car " ++ plate ++ " " ++ renderServiceTypes serviceTypes ++ " " ++ date
renderCommand Lib2.ListCars =
    "list cars"
renderCommand (Lib2.ListServices plate) =
    "list services " ++ plate

renderServiceTypes :: [Lib2.ServiceType] -> String
renderServiceTypes [] = ""
renderServiceTypes [service] = renderServiceType service
renderServiceTypes (service:rest) = renderServiceType service ++ ", " ++ renderServiceTypes rest

renderServiceType :: Lib2.ServiceType -> String
renderServiceType (Lib2.SimpleService name) = name
renderServiceType (Lib2.NestedService name subServices) =
    name ++ "(" ++ renderServiceTypes subServices ++ ")"

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')