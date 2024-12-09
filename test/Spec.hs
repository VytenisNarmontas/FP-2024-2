{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure )

import Lib2 qualified as L2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [ parseQueryTests
    , stateTransitionTests
    ]

-- Tests for parseQuery function
parseQueryTests :: TestTree
parseQueryTests = testGroup "ParseQuery Tests"
    [ 
      -- AddCar Command Tests
      testCase "ParseQuery: Valid AddCar Command" $
        case L2.parseQuery "add car ABC123 Toyota Camry 2020" of
            Right (L2.AddCar plate make model year) -> do
                plate @?= "ABC123"
                make @?= "Toyota"
                model @?= "Camry"
                year @?= 2020
            Left err -> assertFailure $ "Unexpected parse error: " ++ err
      
    , testCase "ParseQuery: AddCar Command with Extra Words" $
        case L2.parseQuery "add car XYZ456 Honda Civic 2019 extra words" of
            Right (L2.AddCar plate make model year) -> do
                plate @?= "XYZ456"
                make @?= "Honda"
                model @?= "Civic"
                year @?= 2019
            Left err -> assertFailure $ "Unexpected parse error: " ++ err
      
    , testCase "ParseQuery: Invalid AddCar Command - Missing Year" $
        case L2.parseQuery "add car DEF789 Ford Mustang" of
            Left _ -> return ()
            Right _ -> assertFailure "Expected parse failure"
      
    , testCase "ParseQuery: Invalid AddCar Command - Invalid Year" $
        case L2.parseQuery "add car DEF789 Ford Mustang YEAR" of
            Left _ -> return ()
            Right _ -> assertFailure "Expected parse failure"
      
      -- RemoveCar Command Tests
    , testCase "ParseQuery: Valid RemoveCar Command" $
        case L2.parseQuery "remove car ABC123" of
            Right (L2.RemoveCar plate) -> plate @?= "ABC123"
            Left err -> assertFailure $ "Unexpected parse error: " ++ err
      
    , testCase "ParseQuery: RemoveCar Command with Extra Words" $
        case L2.parseQuery "remove car XYZ456 extra words" of
            Right (L2.RemoveCar plate) -> plate @?= "XYZ456"
            Left err -> assertFailure $ "Unexpected parse error: " ++ err
      
    , testCase "ParseQuery: Invalid RemoveCar Command" $
        case L2.parseQuery "remove car" of
            Left _ -> return ()
            Right _ -> assertFailure "Expected parse failure"
      
      -- ServiceCar Command Tests
    , testCase "ParseQuery: Valid ServiceCar Command - Simple Service" $
        case L2.parseQuery "service car ABC123 oil-change 2023-01-01" of
            Right (L2.ServiceCar plate services date) -> do
                plate @?= "ABC123"
                services @?= [L2.SimpleService "oil-change"]
                date @?= "2023-01-01"
            Left err -> assertFailure $ "Unexpected parse error: " ++ err
      
    , testCase "ParseQuery: Valid ServiceCar Command - Nested Service" $
        case L2.parseQuery "service car XYZ456 major-service(oil-change, tire-rotation) 2023-02-15" of
            Right (L2.ServiceCar plate services date) -> do
                plate @?= "XYZ456"
                services @?= [L2.NestedService "major-service" 
                    [L2.SimpleService "oil-change, tire-rotation"]]
                date @?= "2023-02-15"
            Left err -> assertFailure $ "Unexpected parse error: " ++ err
      
      -- ListCars Command Tests
    , testCase "ParseQuery: Valid ListCars Command" $
        case L2.parseQuery "list cars" of
            Right L2.ListCars -> return ()
            Left err -> assertFailure $ "Unexpected parse error: " ++ err
      
      -- ListServices Command Tests
    , testCase "ParseQuery: Valid ListServices Command" $
        case L2.parseQuery "list services ABC123" of
            Right (L2.ListServices plate) -> plate @?= "ABC123"
            Left err -> assertFailure $ "Unexpected parse error: " ++ err
    ]

-- Tests for stateTransition function
stateTransitionTests :: TestTree
stateTransitionTests = testGroup "StateTransition Tests"
    [ 
      -- AddCar Transition Tests
      testCase "StateTransition: Add First Car" $
        let initialState = L2.emptyState
            result = L2.stateTransition initialState (L2.AddCar "ABC123" "Toyota" "Camry" 2020)
        in case result of
            Right (msgs, newState) -> do
                length (L2.cars newState) @?= 1
                msgs @?= ["Car added: ABC123"]
            Left err -> assertFailure $ "Unexpected state transition error: " ++ err
      
    , testCase "StateTransition: Add Duplicate Car" $
        let initialState = L2.emptyState
            firstAddResult = L2.stateTransition initialState (L2.AddCar "ABC123" "Toyota" "Camry" 2020)
        in case firstAddResult of
            Right (_, stateAfterFirstAdd) ->
                case L2.stateTransition stateAfterFirstAdd (L2.AddCar "ABC123" "Honda" "Civic" 2021) of
                    Left errorMsg -> errorMsg @?= "Car with this plate already exists"
                    Right _ -> assertFailure "Expected error on duplicate car"
            Left err -> assertFailure $ "First car addition should succeed: " ++ err
      
    , testCase "StateTransition: Remove Existing Car" $
        let initialState = L2.emptyState
            addResult = L2.stateTransition initialState (L2.AddCar "ABC123" "Toyota" "Camry" 2020)
        in case addResult of
            Right (_, stateAfterAdd) ->
                case L2.stateTransition stateAfterAdd (L2.RemoveCar "ABC123") of
                    Right (msgs, newState) -> do
                        length (L2.cars newState) @?= 0
                        msgs @?= ["Car removed: ABC123"]
                    Left err -> assertFailure $ "Removing existing car should succeed: " ++ err
            Left err -> assertFailure $ "First car addition should succeed: " ++ err
      
    , testCase "StateTransition: Remove Non-Existing Car" $
        let initialState = L2.emptyState
        in case L2.stateTransition initialState (L2.RemoveCar "ABC123") of
            Left errorMsg -> errorMsg @?= "Car not found"
            Right _ -> assertFailure "Removing non-existing car should fail"
      
    , testCase "StateTransition: Service Existing Car" $
        let initialState = L2.emptyState
            addResult = L2.stateTransition initialState (L2.AddCar "ABC123" "Toyota" "Camry" 2020)
        in case addResult of
            Right (_, stateAfterAdd) ->
                case L2.stateTransition stateAfterAdd (L2.ServiceCar "ABC123" [L2.SimpleService "oil-change"] "2023-01-01") of
                    Right (msgs, newState) -> do
                        length (L2.services newState) @?= 1
                        msgs @?= ["Car serviced: ABC123"]
                    Left err -> assertFailure $ "Servicing existing car should succeed: " ++ err
            Left err -> assertFailure $ "First car addition should succeed: " ++ err
      
    , testCase "StateTransition: Service Non-Existing Car" $
        let initialState = L2.emptyState
        in case L2.stateTransition initialState (L2.ServiceCar "ABC123" [L2.SimpleService "oil-change"] "2023-01-01") of
            Left errorMsg -> errorMsg @?= "Car not found"
            Right _ -> assertFailure "Servicing non-existing car should fail"
      
    , testCase "StateTransition: List Cars in Empty State" $
        let initialState = L2.emptyState
        in case L2.stateTransition initialState L2.ListCars of
            Right (carsList, _) -> carsList @?= []
            Left err -> assertFailure $ "Listing cars in empty state should not fail: " ++ err
      
    , testCase "StateTransition: List Services for Existing Car" $
        let initialState = L2.emptyState
            addResult = L2.stateTransition initialState (L2.AddCar "ABC123" "Toyota" "Camry" 2020)
        in case addResult of
            Right (_, stateAfterAdd) ->
                let serviceResult = L2.stateTransition stateAfterAdd (L2.ServiceCar "ABC123" [L2.SimpleService "oil-change"] "2023-01-01")
                in case serviceResult of
                    Right (_, stateAfterService) ->
                        case L2.stateTransition stateAfterService (L2.ListServices "ABC123") of
                            Right (servicesList, _) -> length servicesList @?= 1
                            Left err -> assertFailure $ "Listing services for car with service should succeed: " ++ err
                    Left err -> assertFailure $ "Servicing car should succeed: " ++ err
            Left err -> assertFailure $ "First car addition should succeed: " ++ err
      
    , testCase "StateTransition: List Services for Non-Existing Car" $
        let initialState = L2.emptyState
        in case L2.stateTransition initialState (L2.ListServices "ABC123") of
            Left errorMsg -> errorMsg @?= "Car not found"
            Right _ -> assertFailure "Listing services for non-existing car should fail"
    ]