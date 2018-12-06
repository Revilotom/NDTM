import TM
import System.Environment
import Parser
import Control.Monad
import Text.ParserCombinators.ReadP
import Debug.Trace
import Data.Char
import Data.List

main = do
    -- Get the descriptor file path and input file path as command line arguments
    args <- getArgs
    when (length args /= 2)(error "Usage: TM <DescriptionPath> <InputPath>")

    let descFilename  = head args
        inputFilename = last args
    
    -- Remove white space from the input file
    input <- filter (not . isSpace) <$> readFile inputFilename

    -- Parse the descriptor file
    parsedFile <- readP_to_S parseMachineDescriptionFile <$> readFile descFilename

    -- Display parsing errors
    when (null parsedFile)
        (error "Malformed description file")

    let remainder = snd (last parsedFile)

    unless (null remainder)
        (error $ "Parsed up to: " ++ remainder)
    
    -- Run machine and print results
    results <- run descFilename input 
    mapM printInfo results

-- Prints information about the execution of a machine
printInfo :: (Bool, Bool, String, [String]) -> IO ()
printInfo (finished, accepted, tape, p) = do

    if finished then putStrLn "FINISHED" else putStrLn "NOT FINISHED!"

    putStr "Input was "

    when accepted (putStrLn "ACCEPTED")
    when (not accepted && finished) (putStrLn "REJECTED")

    putStrLn ("Tape: " ++ tape)
    putStrLn ("Length: " ++ show (length tape))

    let path = intercalate " -> " p 
    putStrLn ("Moves: "  ++ show( length p))

    putStrLn ("Path: "  ++ show path)
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~"
    return ()
