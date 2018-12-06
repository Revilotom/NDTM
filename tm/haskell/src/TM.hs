module TM where

import Data.List
import Text.ParserCombinators.ReadP
import Debug.Trace
import Data.Maybe
import qualified Data.Map as Map
import System.Environment
import Parser
import Control.Monad

-- Data type to store information concerning the machine
data Machine = Machine {i :: Int, currentState :: String, t :: String, p :: [String], descriptor :: Descriptor, accepted :: Bool,  finished :: Bool} 

-- Run a descriptor file with a given input file
run :: String -> String ->  IO [(Bool, Bool, String, [String])]
run descFilename input = do
    desc <- (fst . last . readP_to_S parseMachineDescriptionFile) <$> readFile descFilename
    let first = head $ intermediate desc
        m = Machine 0 first (input ++ [blank]) [first] desc False False
        results = doMachines [m]
    return results

-- Perform a breadth first search for the accepting state on the tree of execution paths
doMachines :: [Machine] -> [(Bool, Bool, String, [String] )]
doMachines l 
        | null stillGoing || (not . null) success = map getResult success ++ map getResult failed
        | otherwise = doMachines $ concatMap execute l
        where
            stillGoing = filter (not . finished) l
            success = filter accepted l
            failed = filter (not . accepted) l

getResult :: Machine -> (Bool, Bool, String, [String])
getResult m = (finished m, accepted m, t m, p m)


-- Perform the next transition(s) of a machine
execute :: Machine -> [Machine]
execute machine 
        | finished machine = [machine]
        | index > length tape-1 = execute $ updateMachine machine index state (tape ++ [blank])  path
        | index < 0 = execute $ updateMachine machine 0 state (blank : tape)  path
        | not valExists = [Machine index state ("state \"" ++ state1 key ++ "\" does not take the character \"" ++ (tape!!index) : "\" as input.")  path m False True]
        | otherwise = newMachines
        where 
              path = p machine
              m = descriptor machine
              tape = t machine
              index = i machine
              state = currentState machine
              dict = transitions m
              key = Key state (tape!!index)
              valExists = Map.member key dict
              branches = fromJust $ Map.lookup key dict
              possibleTransitions = map getNextStep branches
              newMachines = map (applyTransition machine) possibleTransitions 

-- Apply a transition to a single machine
applyTransition :: Machine -> (String, Char, Char) -> Machine
applyTransition machine transition
        | nextState `elem` accepting m = Machine index state newTape  path m True True
        | nextState `elem` rejecting m = Machine index state newTape  path m False True
        | dir == left  = updateMachine machine (index-1) nextState newTape  path
        | dir == right = updateMachine machine (index+1) nextState newTape  path
        | otherwise = updateMachine machine index nextState newTape  path

        where
            (nextState, dir, newLetter) = transition
            path = p machine ++ [nextState]
            index = i machine
            tape = t machine
            m = descriptor machine
            state = currentState machine
            (leftSide, rightSide) = splitAt index tape
            newTape = leftSide ++ newLetter : tail rightSide

-- Unpack mebers of the value data type
getNextStep :: Value -> (String, Char, Char)
getNextStep val = (state2 val, direction val, output val)

-- Create new machine object from updated parameters
updateMachine :: Machine -> Int -> String -> String -> [String] -> Machine
updateMachine machine index state tape  path = Machine index state tape path (descriptor machine) False False

