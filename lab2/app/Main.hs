module Main (main) where

import BatcherSort
    ( parallelBatcherSort
    , parallelBatcherSortDebug
    )

import GHC.Clock
    ( getMonotonicTimeNSec
    )

import GHC.Conc
    ( getNumCapabilities
    , setNumCapabilities
    )

import System.IO
    ( hFlush
    , stdout
    )

import Text.Printf
    ( printf
    )

import Text.Read
    ( readMaybe
    )


main :: IO ()
main = do
    putStrLn "===== BATCHER ODD-EVEN MERGE SORT ====="
    putStrLn ""

    values <- readArray
    threadCount <- readThreadCount
    debugMode <- readDebugMode

    setNumCapabilities threadCount

    capabilities <- getNumCapabilities

    putStrLn ""
    putStrLn "Original array:"
    print values

    putStrLn ""
    putStrLn $
        "Number of threads: "
            ++ show threadCount

    putStrLn $
        "GHC capabilities: "
            ++ show capabilities

    putStrLn ""

    startTime <- getMonotonicTimeNSec

    sorted <-
        if debugMode
            then
                parallelBatcherSortDebug
                    threadCount
                    values
            else
                parallelBatcherSort
                    threadCount
                    values

    endTime <- getMonotonicTimeNSec

    let elapsedNanoseconds =
            endTime - startTime

        elapsedSeconds =
            fromIntegral elapsedNanoseconds
                / 1000000000.0
                :: Double

    putStrLn ""
    putStrLn "Sorted array:"
    print sorted

    putStrLn ""

    printf
        "Execution time: %.6f seconds\n"
        elapsedSeconds


readArray :: IO [Int]
readArray = do
    putStr "Enter array elements in ONE line separated by spaces: "
    hFlush stdout

    input <- getLine

    case traverse readMaybe (words input) of
        Just values
            | not (null values) ->
                return values

        _ -> do
            putStrLn "Invalid array. Try again."
            readArray


readThreadCount :: IO Int
readThreadCount = do
    putStr "Enter number of threads (k): "
    hFlush stdout

    input <- getLine

    case readMaybe input of
        Just k
            | k > 0 ->
                return k

        _ -> do
            putStrLn "Number of threads must be greater than 0."
            readThreadCount


readDebugMode :: IO Bool
readDebugMode = do
    putStr "Show thread activity? (y/n): "
    hFlush stdout

    input <- getLine

    case input of
        "y" -> return True
        "Y" -> return True
        "n" -> return False
        "N" -> return False

        _ -> do
            putStrLn "Enter y or n."
            readDebugMode