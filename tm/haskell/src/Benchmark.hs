import TM
import Util
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import System.Random (randomRIO, randomR)
import System.Environment
import Parser
import Control.Monad
import Text.ParserCombinators.ReadP
import Debug.Trace
import Data.Csv
import qualified Data.ByteString.Lazy as BS

-- Run benchmark on palindrome Turing machine
benchPalindrome :: IO ()
benchPalindrome = do
    b <- mapM randomPalindrome [1 .. 100]
    outputToFile "palindrome" b

-- Run benchmark on reverse binary language Turing machine
benchRevBinaryLang :: IO ()
benchRevBinaryLang = do
    let tests = map (\x -> 2^x) [1 .. 16]
    let b = map (\x -> getLangString x x (2*x)) tests
    outputToFile "revBinaryLang" b

-- Run benchmark on binary addition Turing machine
benchAddition :: IO ()
benchAddition = do
    let tests = map (\x -> 2^x) [1 .. 62]
    let b = map (\x -> getInputString x x) tests
    outputToFile "binaryAddition" b

-- Run benchmark on multiplication Turing machine
benchMultiplication :: IO ()
benchMultiplication = do
    let tests = [1 .. 20]
    let b = map (\x -> generateInput x x) tests
    outputToFile "multiplier" b

-- Write benchmark to csv file
outputToFile :: String -> [String] -> IO()
outputToFile name b = do
    let machine = TM.run ("../machines/" ++ name)
    (tapeLengths, movesMade) <- unzip . map ((\ (_, _, t, m) -> (length t, length m) ) . head) <$> mapM machine b
    let tapeDifference = zipWith (-) tapeLengths (map length b)
    let lines = zip3 (map length b) tapeDifference movesMade
    let outputFile = "../bench/" ++ name ++ ".csv"
    writeFile outputFile "Length,CellsUsed,Steps\n"
    BS.appendFile outputFile (encode lines)


main = do
    benchPalindrome
    -- benchRevBinaryLang
    -- benchAddition
    -- benchMultiplication
