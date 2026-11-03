module TextTypes
    ( Symbol(..)
    , TextWord(..)
    , PunctuationMark(..)
    , Token(..)
    , Sentence(..)
    , Text(..)
    ) where


newtype Symbol =
    Symbol Char
    deriving (Eq, Show)


newtype TextWord =
    TextWord String
    deriving (Eq, Show)


data PunctuationMark
    = Period
    | Comma
    | Colon
    | Semicolon
    | QuestionMark
    | ExclamationMark
    | Dash
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