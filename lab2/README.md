# Laboratory Work 2 — Parallel Batcher Sort

## Description

This laboratory work implements a multithreaded version of Batcher's
Odd-Even Merge Sort using Haskell.

The number of threads `k` is entered by the user.

The application uses:

- Haskell threads with `forkIO`
- `MVar` for synchronization
- GHC capabilities
- the `-threaded` compiler option
- Batcher Odd-Even Merge Sort
- parallel recursive sorting and merging
- a parallelization threshold to reduce thread-management overhead

## Task

Implement Batcher's exchange sorting with merging for a given array
using `k` threads, where `k` is entered from the keyboard.

## Project Structure

```text
lab2/
├── app/
│   ├── Main.hs
│   └── BatcherSort.hs
├── lab2.cabal
└── README.md
```

## Algorithm

Batcher's Odd-Even Merge Sort is based on recursively dividing an array
into smaller parts.

The program:

1. Reads an array from the keyboard.
2. Reads the number of threads `k`.
3. Extends the array to the next power of two if necessary.
4. Recursively sorts both halves.
5. Merges the sorted halves using Batcher's odd-even merge.
6. Removes additional padding elements.
7. Prints the sorted array.

If the input size is not a power of two, the array is padded internally
with `maxBound` values.

## Multithreading

The application uses `forkIO` to create Haskell threads.

The main thread is counted as one of the available threads. Therefore,
for a value of `k`, at most `k - 1` additional worker threads can operate
at the same time.

`MVar` is used to:

- limit the number of active worker threads;
- synchronize parallel computations;
- receive results from worker threads;
- synchronize debug output.

The number of GHC capabilities is configured with:

```haskell
setNumCapabilities threadCount
```

The project is compiled with:

```text
-threaded
```

which allows Haskell threads to execute in parallel on multiple CPU
cores.

## Parallelization Threshold

Creating a thread for every small recursive operation introduces
significant overhead.

For this reason the program uses:

```haskell
parallelThreshold :: Int
parallelThreshold = 1024
```

Tasks containing 1024 or fewer elements are processed sequentially.

Larger tasks can be divided between multiple threads.

This significantly reduces the overhead caused by excessive `forkIO`
and `MVar` operations.

## Thread Debug Mode

The program can display thread activity:

```text
Show thread activity? (y/n):
```

Example:

```text
[ThreadId 5] START: sorting 8 elements
[ThreadId 6] START: sorting 4 elements
[ThreadId 7] START: sorting 2 elements
[ThreadId 7] END:   sorting 2 elements
```

Different `ThreadId` values demonstrate that computations can be
performed by different Haskell threads.

The total number of thread IDs created during execution can be greater
than `k`, because completed worker threads release their slots and new
threads can later be created.

However, the number of simultaneously active workers is limited.

## Performance Benchmark

The program includes a benchmark mode that compares execution using:

```text
k = 1
k = 2
k = 4
k = 8
```

The benchmark uses a descending array and measures execution time.

The result is fully evaluated using `deepseq` before the final time is
recorded.

Speedup is calculated as:

```text
speedup = execution time with 1 thread / execution time with k threads
```

### Example — 8192 elements

```text
k = 1  | time = 0.099240 seconds | speedup = 1.00x
k = 2  | time = 0.079293 seconds | speedup = 1.25x
k = 4  | time = 0.057778 seconds | speedup = 1.72x
k = 8  | time = 0.054728 seconds | speedup = 1.81x
```

### Example — 32768 elements

```text
k = 1  | time = 0.598090 seconds | speedup = 1.00x
k = 2  | time = 0.495073 seconds | speedup = 1.21x
k = 4  | time = 0.431067 seconds | speedup = 1.39x
k = 8  | time = 0.461617 seconds | speedup = 1.30x
```

The results show that parallel execution can improve performance for
large arrays.

At the same time, increasing the number of threads does not always
increase performance because thread creation, synchronization and
scheduling also require computational resources.

## Build

```powershell
cabal build
```

## Run

```powershell
cabal run
```

Example input:

```text
Enter array elements in ONE line separated by spaces:
8 3 7 4 9 2 6 5

Enter number of threads (k):
4
```

Example result:

```text
Sorted array:
[2,3,4,5,6,7,8,9]
```

## Technologies

- Haskell
- GHC 9.10.3
- Cabal
- `Control.Concurrent`
- `MVar`
- `forkIO`
- `deepseq`
- GHC threaded runtime