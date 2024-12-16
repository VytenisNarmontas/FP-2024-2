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
import Control.Concurrent.STM(STM, TVar)
import qualified Lib2

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = forever $ do
  op <- readChan chan
  case op of
    Save content replyChan -> 
      withFile "state.txt" WriteMode $ \h -> do
        hPutStrLn h content
        writeChan replyChan ()
    Load replyChan -> 
      withFile "state.txt" ReadMode $ \h -> do
        content <- hGetContents h
        writeChan replyChan content

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input
  | "save" `isPrefixOf` input = Right (SaveCommand, drop 4 input)
  | "load" `isPrefixOf` input = Right (LoadCommand, drop 4 input)
  | "begin" `isPrefixOf` input = parseBatchStatements (drop 5 input)
  | otherwise = fmap (first StatementCommand) (parseStatements input)

parseBatchStatements :: String -> Either String (Command, String)
parseBatchStatements input = do
  (statements, rest) <- parseMultipleStatements input
  pure (StatementCommand (Batch statements), rest)

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input = 
  case Lib2.parseQuery input of
    Right query -> Right (Single query, "")
    Left _ -> 
      case input of
        "end" -> Left "Empty batch"
        _ -> Left "Unable to parse statement"

parseMultipleStatements :: String -> Either String ([Lib2.Query], String)
parseMultipleStatements input = go [] input
  where
    go acc [] = Left "Unclosed batch"
    go acc ('e':'n':'d':rest) = Right (reverse acc, rest)
    go acc str = 
      case Lib2.parseQuery str of
        Right (query, remaining) -> 
          case remaining of
            (';':xs) -> go (query:acc) (dropWhile (==' ') xs)
            "end" -> Right (reverse (query:acc), "end")
            _ -> Left "Invalid batch syntax"
        Left _ -> Left "Unable to parse query in batch"

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State cars services) = 
  Batch $ carQueries ++ serviceQueries
  where
    carQueries = map toAddCarQuery cars
    serviceQueries = map toServiceQuery services

    toAddCarQuery (Lib2.Car plate make model year) = 
      Lib2.AddCar plate make model year
    
    toServiceQuery (Lib2.Service plate services date) = 
      Lib2.ServiceCar plate services date

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) = 
  "begin " ++ intercalate "; " (map renderQuery queries) ++ " end"

renderQuery :: Lib2.Query -> String
renderQuery (Lib2.AddCar plate make model year) = 
  printf "add car %s %s %s %d" plate make model year
renderQuery (Lib2.RemoveCar plate) = 
  "remove car " ++ plate
renderQuery (Lib2.ServiceCar plate services date) = 
  printf "service car %s %s %s" plate 
    (intercalate ", " (map show services)) date

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> 
                   IO (Either String (Maybe String))
stateTransition stateVar cmd ioChan = atomically $ do
  currentState <- readTVar stateVar
  case cmd of
    SaveCommand -> do
      let statements = marshallState currentState
          content = renderStatements statements
      liftIO $ do
        replyChan <- newChan
        writeChan ioChan (Save content replyChan)
        readChan replyChan
        return (Right (Just "State saved"))
    
    LoadCommand -> do
      liftIO $ do
        replyChan <- newChan
        writeChan ioChan (Load replyChan)
        content <- readChan replyChan
        case parseStatements content of
          Right (Batch queries, _) -> do
            finalState <- foldM applyQuery Lib2.emptyState queries
            writeTVarIO stateVar finalState
            return (Right (Just "State loaded"))
          _ -> return (Left "Failed to parse saved state")
    
    StatementCommand statements -> do
      result <- processStatements currentState statements
      case result of 
        Right (message, newState) -> do
          writeTVar stateVar newState
          return (Right message)
        Left err -> return (Left err)

processStatements :: Lib2.State -> Statements -> STM (Either String (Maybe String, Lib2.State))
processStatements state (Single query) = do
  result <- applyQuery state query
  return $ Right (Just ("Executed: " ++ show query), result)
processStatements state (Batch queries) = do
  result <- foldM applyQuery state queries
  return $ Right (Just ("Batch of " ++ show (length queries) ++ " queries executed"), result)

applyQuery :: Lib2.State -> Lib2.Query -> STM Lib2.State
applyQuery state query = do
  case Lib2.stateTransition state query of
    Right (_, newState) -> return newState
    Left err -> error err  -- Or handle error appropriately
