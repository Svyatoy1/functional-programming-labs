module TextProcessor
    ( normalizeWhitespace
    , extractWords
    , startsWithVowel
    , sortVowelWords
    ) where

import Data.Char
    ( isAlpha
    , toLower
    )

import Data.List
    ( sortBy
    )

import TextTypes
    ( TextWord(..)
    )


normalizeWhitespace :: String -> String
normalizeWhitespace =
    unwords . words


extractWords :: String -> [TextWord]
extractWords text =
    map TextWord $
        words $
            map replaceNonLetter text
  where
    replaceNonLetter character
        | isAlpha character = character
        | character == '\'' = character
        | character == '’' = character
        | character == '-' = character
        | otherwise = ' '


startsWithVowel :: TextWord -> Bool
startsWithVowel (TextWord []) =
    False

startsWithVowel (TextWord (firstCharacter : _)) =
    toLower firstCharacter `elem` vowels
  where
    vowels =
        "aeiouаеєиіїоуюя"


sortVowelWords :: [TextWord] -> [TextWord]
sortVowelWords =
    sortBy compareFirstLetter
        . filter startsWithVowel
  where
    compareFirstLetter
        (TextWord (firstCharacter : _))
        (TextWord (secondCharacter : _)) =
            compare
                (toLower firstCharacter)
                (toLower secondCharacter)

    compareFirstLetter _ _ =
        EQ