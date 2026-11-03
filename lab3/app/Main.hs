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
    , parseText
    , renderText
    , sortVowelWords
    )

import TextTypes
    ( Text(..)
    , TextWord
    , wordToString
    )


main :: IO ()
main = do
    hSetEncoding stdout utf8

    handle <- openFile "textbook.txt" ReadMode
    hSetEncoding handle utf8

    originalText <- hGetContents handle

    let normalizedText =
            normalizeWhitespace originalText

        parsedText =
            parseText normalizedText

        restoredText =
            renderText parsedText

        allWords =
            extractWords parsedText

        vowelWords =
            sortVowelWords allWords

    putStrLn "===== TEXT PROCESSOR ====="
    putStrLn ""

    putStrLn "Normalized text:"
    putStrLn normalizedText

    putStrLn ""
    putStrLn "Text formed from custom data types:"
    putStrLn restoredText

    putStrLn ""
    putStrLn $
        "Number of sentences: "
            ++ show (sentenceCount parsedText)

    putStrLn ""
    putStrLn "Words beginning with vowels:"
    printWords vowelWords


sentenceCount :: Text -> Int
sentenceCount (Text sentences) =
    length sentences


printWords :: [TextWord] -> IO ()
printWords =
    mapM_ (putStrLn . wordToString)