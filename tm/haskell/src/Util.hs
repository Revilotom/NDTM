module Util where

import TM
import Parser
import Data.List.Split
import Control.Monad
import System.Random
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

-- Generates an input string for the binary addition Turing machine
getInputString :: Int -> Int -> String
getInputString x y = getBinaryRepresentation x ++ [blank] ++ getBinaryRepresentation y

-- Generates a random string from valid palindrome characters
randomString :: Int -> IO String
randomString n = Control.Monad.replicateM n (randomRIO ('0','2'::Char))

-- Generates a random palindrome
randomPalindrome :: Int -> IO String
randomPalindrome n = do
            str <- randomString n
            return (str ++ reverse str)

-- Generates input for the multiplication Turing machine
generateInput :: Int -> Int -> String
generateInput x y = replicate x '0' ++ ['1'] ++ replicate y '0'

-- Generates binary representation of an integer
getBinaryRepresentation :: Int -> String
getBinaryRepresentation num = showIntAtBase 2 intToDigit num ""

-- Generates string for the reverse binary language
getLangString :: Int -> Int -> Int ->  String
getLangString x y sum = a ++ "#" ++ b ++ "#" ++ c
        where
                a = reverse $ getBinaryRepresentation x
                b = reverse $ getBinaryRepresentation y
                c = reverse $ getBinaryRepresentation sum
