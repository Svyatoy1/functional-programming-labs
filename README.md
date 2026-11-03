# Functional Programming Labs

Laboratory works for the **Functional Programming** course implemented in **Haskell**.

## Repository Structure

```text
functional_programming_labs/
├── lab1/
├── lab2/
├── lab3/
└── README.md
```

## Laboratory Work 1 — Faculty Sports Information System

A console information system for managing sports activities at a faculty.

Main features:

- Haskell classes and instances
- MySQL database integration
- CRUD operations
- students and teachers
- sport sections
- schedules
- competitions
- section members
- competition participants
- UTF-8 console and database support

Technologies:

- Haskell
- GHC
- Cabal
- MySQL
- `mysql-haskell`
- `Haskeline`

Detailed information is available in:

```text
lab1/README.md
```

## Laboratory Work 2 — Parallel Batcher Sort

A multithreaded implementation of **Batcher's Odd-Even Merge Sort**.

Main features:

- configurable number of threads `k`
- `forkIO`
- `MVar` synchronization
- GHC capabilities
- `-threaded` runtime
- thread activity debug mode
- parallelization threshold
- execution time measurement
- performance benchmark
- speedup calculation

Technologies:

- Haskell
- GHC
- Cabal
- `Control.Concurrent`
- `MVar`
- `deepseq`

Detailed information is available in:

```text
lab2/README.md
```

## Laboratory Work 3 — Text Processing

A program for processing a programming textbook stored in a file.

Main features:

- custom types for symbols, words, sentences and punctuation
- text parsing
- whitespace normalization
- reconstruction of text from custom data types
- sentence detection
- extraction of words beginning with vowels
- sorting words by their first letter
- English and Ukrainian vowel support

Technologies:

- Haskell
- GHC
- Cabal
- algebraic data types
- pattern matching
- recursion
- list processing
- file I/O

Detailed information is available in:

```text
lab3/README.md
```

## Build and Run

Each laboratory work is an independent Cabal project.

Open the required directory:

```powershell
cd lab1
```

or:

```powershell
cd lab2
```

or:

```powershell
cd lab3
```

Then build and run:

```powershell
cabal build
cabal run
```

## Environment

The laboratory works were developed using:

- GHC 9.10.3
- Cabal
- Visual Studio Code
- Windows