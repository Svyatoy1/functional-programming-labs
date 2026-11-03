module Main (main) where

import System.IO
    ( IOMode(ReadMode)
    , hGetContents
    , hSetEncoding
    , openFile
    , stdout
    , utf8
    )

import TextProcessor
    ( extractWords
    , normalizeWhitespace
    , sortVowelWords
    )

import TextTypes
    ( TextWord(..)
    )


main :: IO ()
main = do
    hSetEncoding stdout utf8

    handle <- openFile "textbook.txt" ReadMode
    hSetEncoding handle utf8

    originalText <- hGetContents handle

    let normalizedText =
            normalizeWhitespace originalText

        allWords =
            extractWords normalizedText

        vowelWords =
            sortVowelWords allWords

    putStrLn "===== TEXT PROCESSOR ====="
    putStrLn ""

    putStrLn "Normalized text:"
    putStrLn normalizedText

    putStrLn ""
    putStrLn "Words beginning with vowels:"
    printWords vowelWords


printWords :: [TextWord] -> IO ()
printWords =
    mapM_ printWord
  where
    printWord (TextWord word) =
        putStrLn word