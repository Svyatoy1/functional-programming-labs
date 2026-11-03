module TextTypes
    ( Symbol(..)
    , TextWord(..)
    , PunctuationMark(..)
    , Token(..)
    , Sentence(..)
    , Text(..)
    , stringToWord
    , wordToString
    ) where


newtype Symbol =
    Symbol Char
    deriving (Eq, Show)


newtype TextWord =
    TextWord [Symbol]
    deriving (Eq, Show)


data PunctuationMark
    = Period
    | Comma
    | Colon
    | Semicolon
    | QuestionMark
    | ExclamationMark
    | Dash
    | Slash
    | Quote
    | ParenthesisOpen
    | ParenthesisClose
    | OtherPunctuation Char
    deriving (Eq, Show)


data Token
    = WordToken TextWord
    | PunctuationToken PunctuationMark
    deriving (Eq, Show)


newtype Sentence =
    Sentence [Token]
    deriving (Eq, Show)


newtype Text =
    Text [Sentence]
    deriving (Eq, Show)


stringToWord :: String -> TextWord
stringToWord =
    TextWord . map Symbol


wordToString :: TextWord -> String
wordToString (TextWord symbols) =
    map symbolToChar symbols
  where
    symbolToChar (Symbol character) =
        character