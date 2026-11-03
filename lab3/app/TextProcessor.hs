module TextProcessor
    ( normalizeWhitespace
    , parseText
    , renderText
    , extractWords
    , startsWithVowel
    , sortVowelWords
    ) where

import Data.Char
    ( isAlpha
    , isSpace
    , toLower
    )

import Data.List
    ( sortBy
    )

import TextTypes
    ( PunctuationMark(..)
    , Sentence(..)
    , Symbol(..)
    , Text(..)
    , TextWord(..)
    , Token(..)
    , stringToWord
    , wordToString
    )


normalizeWhitespace :: String -> String
normalizeWhitespace =
    unwords . words


parseText :: String -> Text
parseText =
    Text . splitSentences . tokenize


renderText :: Text -> String
renderText (Text sentences) =
    unwords (map renderSentence sentences)


renderSentence :: Sentence -> String
renderSentence (Sentence tokens) =
    renderTokens tokens


renderTokens :: [Token] -> String
renderTokens [] =
    ""

renderTokens [token] =
    tokenToString token

renderTokens (token : nextToken : rest) =
    tokenToString token
        ++ tokenSeparator token nextToken
        ++ renderTokens (nextToken : rest)


tokenSeparator :: Token -> Token -> String
tokenSeparator currentToken nextToken
    | noSpaceBefore nextToken =
        ""

    | noSpaceAfter currentToken =
        ""

    | otherwise =
        " "


noSpaceBefore :: Token -> Bool
noSpaceBefore token =
    case token of
        PunctuationToken Period ->
            True

        PunctuationToken Comma ->
            True

        PunctuationToken Colon ->
            True

        PunctuationToken Semicolon ->
            True

        PunctuationToken QuestionMark ->
            True

        PunctuationToken ExclamationMark ->
            True

        PunctuationToken ParenthesisClose ->
            True

        _ ->
            False


noSpaceAfter :: Token -> Bool
noSpaceAfter token =
    case token of
        PunctuationToken ParenthesisOpen ->
            True

        _ ->
            False


tokenToString :: Token -> String
tokenToString token =
    case token of
        WordToken word ->
            wordToString word

        PunctuationToken punctuation ->
            punctuationToString punctuation


punctuationToString :: PunctuationMark -> String
punctuationToString punctuation =
    case punctuation of
        Period ->
            "."

        Comma ->
            ","

        Colon ->
            ":"

        Semicolon ->
            ";"

        QuestionMark ->
            "?"

        ExclamationMark ->
            "!"

        Dash ->
            "-"

        Quote ->
            "\""

        ParenthesisOpen ->
            "("

        ParenthesisClose ->
            ")"

        OtherPunctuation character ->
            [character]


tokenize :: String -> [Token]
tokenize [] =
    []

tokenize (character : rest)
    | isAlpha character =
        let
            (wordRest, remainingText) =
                span isWordCharacter rest

            word =
                character : wordRest
        in
            WordToken (stringToWord word)
                : tokenize remainingText

    | isSpace character =
        tokenize rest

    | otherwise =
        PunctuationToken (toPunctuation character)
            : tokenize rest


isWordCharacter :: Char -> Bool
isWordCharacter character =
    isAlpha character
        || character == '\''
        || character == '’'
        || character == '-'


toPunctuation :: Char -> PunctuationMark
toPunctuation character =
    case character of
        '.' -> Period
        ',' -> Comma
        ':' -> Colon
        ';' -> Semicolon
        '?' -> QuestionMark
        '!' -> ExclamationMark
        '-' -> Dash
        '"' -> Quote
        '(' -> ParenthesisOpen
        ')' -> ParenthesisClose
        _   -> OtherPunctuation character


splitSentences :: [Token] -> [Sentence]
splitSentences =
    go []
  where
    go currentTokens []
        | null currentTokens =
            []

        | otherwise =
            [Sentence (reverse currentTokens)]

    go currentTokens (token : rest) =
        let
            updatedTokens =
                token : currentTokens
        in
            if isSentenceEnd token
                then
                    Sentence (reverse updatedTokens)
                        : go [] rest
                else
                    go updatedTokens rest


isSentenceEnd :: Token -> Bool
isSentenceEnd token =
    case token of
        PunctuationToken Period ->
            True

        PunctuationToken QuestionMark ->
            True

        PunctuationToken ExclamationMark ->
            True

        _ ->
            False


extractWords :: Text -> [TextWord]
extractWords (Text sentences) =
    concatMap wordsFromSentence sentences
  where
    wordsFromSentence (Sentence tokens) =
        [ word
        | WordToken word <- tokens
        ]


startsWithVowel :: TextWord -> Bool
startsWithVowel (TextWord []) =
    False

startsWithVowel (TextWord (Symbol firstCharacter : _)) =
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
        (TextWord (Symbol firstCharacter : _))
        (TextWord (Symbol secondCharacter : _)) =
            compare
                (toLower firstCharacter)
                (toLower secondCharacter)

    compareFirstLetter _ _ =
        EQ