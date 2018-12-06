module Parser where

import qualified Data.Map as Map
import Data.List
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative
import Control.Monad

blank :: Char
blank = '_'

accept:: Char
accept = '+'

reject:: Char
reject = '+'

left:: Char
left = 'L'

right:: Char
right = 'R'

still:: Char
still = 'S'

-- Data type representing the key in the map of transitions
data Key = Key {state1 :: String, input :: Char} deriving (Show, Ord, Eq)

-- Data type representing the value in the map of transitions
data Value = Value {state2 :: String, output :: Char, direction :: Char} deriving (Show)

-- Data type representing the parsed descriptor file
data Descriptor = Descriptor {transitions:: Map.Map Key [Value], intermediate :: [String],
accepting :: [String], rejecting :: [String]} deriving (Show)

-- Converts a list to a map without removing duplicate keys (as is the case for Map.fromList)
convertKVList :: (Ord a) => [(a, b)] -> Map.Map a [b]
convertKVList ls = Map.fromListWith (++) . map (\(x,y) -> (x,[y])) $ ls

-- Define parser combinators

digit :: ReadP Char
digit = satisfy isDigit

stateNameChar :: ReadP Char
stateNameChar = satisfy isAlphaNum

anyCharacter :: ReadP Char
anyCharacter = satisfy (not . isSeparator)

-- A letter in the alphabet line
letter :: ReadP String
letter = do
    char <- count 1 anyCharacter
    string " " <|> string "\n"
    return char

-- A state declaration line
state :: ReadP String
state = do
    name <- many1 stateNameChar
    qualifier <- string "\n" <|> string " -\n" <|> string " +\n"
    return $ name ++ qualifier

-- A line in the transition table
transition :: [String] -> [String] -> [String] -> [String] ->  ReadP (Key, Value)
transition alphabet intermediate accepting rejecting = do
    state1 <- many1 stateNameChar
    string " "
    input <- count 1 anyCharacter
    string " "
    state2 <- many1 stateNameChar
    string " "
    output <- count 1 anyCharacter
    string " "
    direction <- string [right] <|> string [left] <|> string [still]
    string "\n"
    unless (state1 `elem` intermediate) pfail
    unless (state2 `elem` (intermediate++rejecting++accepting)) pfail
    unless (input  `elem` ([blank]:alphabet)) pfail
    unless (output `elem` ([blank]:alphabet)) pfail
    return (Key state1 (head input), Value state2 (head output) (head direction))

-- Parses up to the alphabet line
parseStates :: ReadP ([String], [String], [String])
parseStates = do
    string "states "
    stateCount <- fmap read (many1 digit)
    string "\n"
    states <- count stateCount state
    let rejecting = [filter isAlphaNum x | x <- states, reject `elem` x]
    let accepting = [filter isAlphaNum x | x <- states, accept `elem` x]
    let intermediate = ([filter isAlphaNum x | x <- states] \\ rejecting) \\ accepting
    return (intermediate, accepting, rejecting)

-- Parses alphabet line
parseAlphabet :: ReadP [String]
parseAlphabet = do
    string "alphabet "
    letterCount <- fmap readInt (many1 digit)
    string " "
    count letterCount letter

-- Parses the transition table
parseTransitions :: [String] -> [String] -> [String] -> [String] -> ReadP (Map.Map Key [Value])
parseTransitions alphabet intermediate accepting rejecting = do
    transitionsList <- many1 $ transition alphabet intermediate accepting rejecting
    return $ convertKVList transitionsList

-- Parses the entire descriptor file
parseMachineDescriptionFile :: ReadP Descriptor
parseMachineDescriptionFile = do
    (intermediate, accepting, rejecting) <- parseStates
    alphabet <- parseAlphabet
    transitions <- parseTransitions alphabet intermediate accepting rejecting
    return $ Descriptor transitions intermediate accepting rejecting

-- Syntatic sugar
readInt :: String -> Int
readInt = read
