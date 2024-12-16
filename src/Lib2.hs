module Lib2
    ( parseQuery
    , emptyState
    , stateTransition
    , Command(..)
    , Car(..)
    , Service(..)
    , ServiceType(..)
    , State(..)
    ) where

import Data.List (find, delete)
import Data.Char (isAlpha, isDigit, isAlphaNum)

type Parser a = String -> Either String (a, String)

and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 f a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right (f v1 v2, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1

or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left (e1 ++ ", " ++ e2)

parseString :: String -> Parser String
parseString str [] = Left ("Cannot find " ++ str ++ " in an empty input")
parseString str input 
    | take (length str) input == str = Right (str, drop (length str) input)
    | otherwise = Left ("Cannot find " ++ str ++ " in " ++ input)

parsePlate :: Parser String
parsePlate [] = Left "Empty input, cannot parse plate"
parsePlate input =
    let (plate, rest) = span isAlphaNum input
    in if null plate 
        then Left "Invalid plate format"
        else Right (plate, rest)

-- Each car has a unique plate number, make, model, and year of manufacture
data Car = Car
  { carPlate :: String  -- (e.g., ABC123)
  , carMake  :: String  -- (e.g., Toyota, Ford)
  , carModel :: String  -- (e.g., Camry, Mustang)
  , carYear  :: Int     -- (e.g., 2020, 1995)
  } deriving (Show, Eq) 

-- Represents a service for a car
data Service = Service
  { serviceCarPlate :: String       -- License plate (e.g., ABC123)
  , serviceTypes    :: [ServiceType] -- List of services performed
  , serviceDate     :: String        -- Date of the service
  } deriving (Show)

-- Service type representation supporting simple and nested services
data ServiceType 
  = SimpleService String            -- Basic service with a single type
  | NestedService String [ServiceType]  -- Complex service with sub-services
  deriving (Eq)

-- Custom Show instance to provide readable representation of ServiceType
instance Show ServiceType where
    show (SimpleService s) = s
    show (NestedService s services) = 
        s ++ "(" ++ showServices services ++ ")"

-- Helper function to convert a list of services to a readable string
showServices :: [ServiceType] -> String
showServices [] = ""
showServices [x] = show x 
showServices (x:xs) = show x ++ ", " ++ showServices xs

-- Tracking all cars and their service history
data State = State
  { cars     :: [Car]      -- List of all cars in the system
  , services :: [Service]  -- List of all service records
  } deriving (Show)

-- Defines possible commands that can be executed in the system
data Command
  = AddCar String String String Int         -- Add a new car (plate, make, model, year)
  | RemoveCar String                        -- Remove a car by plate
  | ServiceCar String [ServiceType] String  -- Service a car (plate, services, date)
  | ListCars                                -- List all cars
  | ListServices String                     -- List services for a specific car
  deriving (Show, Eq)

-- Function that applies commands and manages system state
stateTransition :: State -> Command -> Either String ([String], State)
stateTransition state@(State oldCars oldServices) cmd = case cmd of
    -- Prevents duplicate plates
    AddCar plate make model year -> 
        case findCar plate oldCars of
            Just _ -> Left "Car with this plate already exists"
            Nothing -> Right (["Car added: " ++ plate], State (newCar : oldCars) oldServices)
                where newCar = Car plate make model year
    
    -- Ensures car exists before removal
    RemoveCar plate -> 
        case findCar plate oldCars of
            Nothing -> Left "Car not found"
            Just car -> Right (["Car removed: " ++ plate], State (delete car oldCars) oldServices)

    -- Adding a service recddord to a car
    ServiceCar plate serviceTypes date -> 
        case findCar plate oldCars of
            Nothing -> Left "Car not found"
            Just _ -> Right (["Car serviced: " ++ plate], State oldCars (Service plate serviceTypes date : oldServices))

    -- List all cars in the system
    ListCars -> 
        Right (map show oldCars, state)

    -- List services for a car
    ListServices plate -> 
        case findCar plate oldCars of
            Nothing -> Left "Car not found"
            Just _ -> Right (map show $ filter (\s -> serviceCarPlate s == plate) oldServices, state)

-- Function to find a car by its plate
findCar :: String -> [Car] -> Maybe Car
findCar plate = find (\car -> carPlate car == plate)

-- Parses commands to remove a car
parseRemoveCar :: String -> Either String Command
parseRemoveCar input = 
    let parser = and2 (\_ plate -> RemoveCar plate) 
                 (parseString "remove car ")
                 (parsePlate `or2` parseRemainderAsPlate)
    in case parser input of
        Right (cmd, _) -> Right cmd  -- Ignore any trailing input
        Left err -> Left err

parseRemainderAsPlate :: Parser String
parseRemainderAsPlate input = 
    let (plate, rest) = span isAlphaNum input
    in if null plate 
        then Left "Invalid plate format"
        else Right (plate, rest)

-- Central parsing function that attempts to parse various command types
parseQuery :: String -> Either String Command
parseQuery input = parseAddCar input
    `orElse` parseRemoveCar input
    `orElse` parseServiceCar input
    `orElse` parseListCars input
    `orElse` parseListServices input
  where
    -- Custom combinator to chain parsing attempts, returning first successful parse
    orElse :: Either String Command -> Either String Command -> Either String Command
    orElse (Right res) _ = Right res 
    orElse _ (Right res) = Right res
    orElse (Left msg1) (Left msg2) = Left (msg1 ++ " or " ++ msg2)  -- Combine error messages
    orElse _ _ = Left "Invalid command"

-- Parses commands to add a new car
parseAddCar :: String -> Either String Command
parseAddCar input =
    case words input of
        ("add" : "car" : plate : make : model : yearStr : _) -> 
            case reads yearStr of
                [(year, "")] -> Right (AddCar plate make model year)
                _ -> Left "Invalid year format"
        _ -> Left "Invalid add car command"

-- Parser for car nested service types
parseServiceCar :: String -> Either String Command
parseServiceCar input = 
    case words input of
        ("service" : "car" : plate : rest) -> 
            let (serviceStr, dateStr) = splitAtLastSpace (unwords rest)
                services = parseServiceList serviceStr
            in if not (null dateStr)
                then Right (ServiceCar plate services (trim dateStr))
                else Left $ "Invalid service car command: missing date. serviceStr: " ++ serviceStr ++ ", dateStr: " ++ dateStr
        _ -> Left "Invalid service car command"

-- Converts a string of services into a list of ServiceType
parseServiceList :: String -> [ServiceType]
parseServiceList input = 
    sanitizeParsedServices $ map (fst . parseServiceType . trim) (splitTopLevelServices input)

-- Remove extra parentheses and whitespace
sanitizeParsedServices :: [ServiceType] -> [ServiceType]
sanitizeParsedServices = map sanitizeServiceType

-- Recursively sanitizes a single service type
sanitizeServiceType :: ServiceType -> ServiceType
sanitizeServiceType (SimpleService s) = SimpleService (trimUnmatched s)
sanitizeServiceType (NestedService s services) = 
    NestedService (trimUnmatched s) (sanitizeParsedServices services)

-- Removes unmatched parentheses from the beginning and end of a string
trimUnmatched :: String -> String
trimUnmatched = reverse . dropWhile (== ')') . reverse . dropWhile (== '(')

-- Splits input string at the last space, returning services and date
splitAtLastSpace :: String -> (String, String)
splitAtLastSpace input =
    case breakAtLastSpace input of
        (before, after) -> (trim before, trim after)

-- Breaks a string at the last space, reversing to handle from the end
breakAtLastSpace :: String -> (String, String)
breakAtLastSpace str =
    let reversed = reverse str
        (revAfter, revBefore) = break (== ' ') reversed
    in (reverse revBefore, reverse revAfter)

-- Parser to split service types
splitTopLevelServices :: String -> [String]
splitTopLevelServices input = split 0 "" [] input
  where
    split _ curr acc [] = if null curr then acc else acc ++ [reverse curr]
    split depth curr acc (c:cs)
        | c == '(' = split (depth + 1) (c:curr) acc cs  -- Increase depth for nested parentheses
        | c == ')' = split (depth - 1) (c:curr) acc cs  -- Decrease depth
        | c == ',' && depth == 0 = split depth "" (acc ++ [reverse curr]) cs  -- Split at top-level commas
        | otherwise = split depth (c:curr) acc cs

-- Removes leading and trailing whitespace from a string
trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Parses individual service types, handling both simple and nested formats
parseServiceType :: String -> (ServiceType, String)
parseServiceType input = 
    let input' = trim input
    in case break (== '(') input' of
        (name, "") -> (SimpleService (trim name), "")  -- Simple service case
        (name, '(':rest) -> 
            let (services, remaining) = parseNestedList rest 1
            in (NestedService (trim name) services, trim remaining)
        _ -> error $ "Unexpected service format: " ++ input

-- Recursively parses nested service lists
parseNestedList :: String -> Int -> ([ServiceType], String)
parseNestedList input depth = go (trim input) depth []
  where
    go [] _ acc = (reverse acc, []) 
    go (')':rest) 1 acc = (reverse acc, trim rest)
    go (')':rest) d acc = go rest (d - 1) acc 
    go ('(':rest) d acc = 
        let (service, remaining) = parseServiceType ('(':rest)
        in go remaining (d + 1) (service : acc)  
    go (',':rest) d acc = go rest d acc 
    go str d acc = 
        let (service, remaining) = parseServiceType str
        in go remaining d (service : acc) 

-- Parses commands to list all cars
parseListCars :: String -> Either String Command
parseListCars input =
    case words input of
        ("list" : "cars" : _) -> Right ListCars
        _ -> Left "Invalid list cars command"

-- Parses commands to list services for a specific car
parseListServices :: String -> Either String Command
parseListServices input =
    case words input of
        ("list" : "services" : plate : _) -> Right (ListServices plate)
        _ -> Left "Invalid list services command"

-- Creates an initial empty state with no cars or services
emptyState :: State
emptyState = State [] []