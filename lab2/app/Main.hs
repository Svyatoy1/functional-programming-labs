module Main (main) where

import BatcherSort (batcherSort)


main :: IO ()
main = do
    putStrLn "===== BATCHER ODD-EVEN MERGE SORT ====="
    putStrLn ""
    putStrLn "Enter array elements separated by spaces:"

    input <- getLine

    let values = map read (words input) :: [Int]

    if null values
        then
            putStrLn "Array is empty."
        else do
            putStrLn ""
            putStrLn "Original array:"
            print values

            let sorted = batcherSort values

            putStrLn ""
            putStrLn "Sorted array:"
            print sorted