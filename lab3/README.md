# Laboratory Work 3 — Text Processing in Haskell

## Description

This laboratory work implements a text processing application in Haskell.

The program reads a fragment of a programming textbook from a file, converts it into custom data types, normalizes whitespace, processes the words and forms the text back from its internal representation.

The main task is to find all words beginning with vowels and sort them alphabetically by the first letter.

## Task

Create a Haskell program for processing the text of a programming textbook stored in a file.

The program must use custom types such as:

- Symbol
- Word
- Sentence
- Punctuation Mark
- Text

Tabs and sequences of multiple whitespace characters must be replaced with a single space.

Words beginning with vowels must be sorted alphabetically by their first letter.

## Project Structure

```text
lab3/
├── app/
│   ├── Main.hs
│   ├── TextTypes.hs
│   └── TextProcessor.hs
├── textbook.txt
├── lab3.cabal
└── README.md
```

## Custom Data Types

The application represents the text using custom Haskell types.

```haskell
newtype Symbol =
    Symbol Char
```

A word consists of symbols:

```haskell
newtype TextWord =
    TextWord [Symbol]
```

Punctuation marks are represented by a separate algebraic data type:

```haskell
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
```

A token can contain either a word or a punctuation mark:

```haskell
data Token
    = WordToken TextWord
    | PunctuationToken PunctuationMark
```

A sentence consists of tokens:

```haskell
newtype Sentence =
    Sentence [Token]
```

The complete text consists of sentences:

```haskell
newtype Text =
    Text [Sentence]
```

Therefore, the internal structure of the document is:

```text
Text
└── Sentence
    ├── WordToken
    │   └── TextWord
    │       └── Symbol
    └── PunctuationToken
        └── PunctuationMark
```

## Whitespace Normalization

Before parsing, whitespace is normalized using:

```haskell
normalizeWhitespace :: String -> String
normalizeWhitespace =
    unwords . words
```

This converts tabs, line breaks and sequences of spaces into a single space.

For example:

```text
An    algorithm	is useful.
```

becomes:

```text
An algorithm is useful.
```

## Text Parsing

The normalized string is parsed into the custom `Text` structure.

The parser recognizes:

- words;
- periods;
- commas;
- colons;
- semicolons;
- question marks;
- exclamation marks;
- parentheses;
- quotes;
- slashes;
- other punctuation.

Sentence boundaries are recognized using:

```text
.
?
!
```

Hyphens inside words are preserved, so a word such as:

```text
Object-oriented
```

remains one word.

## Text Reconstruction

The program can reconstruct the text from the custom data types using:

```haskell
renderText :: Text -> String
```

Therefore, the application performs the complete transformation:

```text
textbook.txt
    ↓
String
    ↓
normalization
    ↓
Text / Sentence / Token / TextWord / Symbol
    ↓
processing
    ↓
renderText
    ↓
String
```

This demonstrates that the custom data types are actually used for text processing rather than only declared.

## Words Beginning with Vowels

The program supports English and Ukrainian vowels.

English:

```text
a e i o u
```

Ukrainian:

```text
а е є и і ї о у ю я
```

Letter case is ignored when checking the first character.

For example, both:

```text
algorithm
Array
Input
Object
```

are recognized as words beginning with vowels.

## Sorting

Words beginning with vowels are sorted by their first letter.

The comparison is case-insensitive.

For example:

```text
Array
algorithm
Each
input
Object
using
```

are grouped according to their initial letters:

```text
A
E
I
O
U
```

Words with the same first letter keep their relative order because the task requires sorting by the first letter rather than by the complete word.

## Input File

The input text is stored in:

```text
textbook.txt
```

The file contains a fragment of a programming textbook.

## Build

From the `lab3` directory:

```powershell
cabal build
```

## Run

```powershell
cabal run
```

## Program Output

The application displays:

1. normalized source text;
2. text reconstructed from the custom data types;
3. number of sentences;
4. words beginning with vowels sorted by their first letter.

Example:

```text
===== TEXT PROCESSOR =====

Normalized text:
Programming languages are used ...

Text formed from custom data types:
Programming languages are used ...

Number of sentences: 10

Words beginning with vowels:
are
algorithms
and
applications
An
algorithm
...
```

## Technologies

- Haskell
- GHC 9.10.3
- Cabal
- algebraic data types
- pattern matching
- recursion
- list processing
- file I/O