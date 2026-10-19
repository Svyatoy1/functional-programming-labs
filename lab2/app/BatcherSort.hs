module BatcherSort
    ( batcherSort
    , parallelBatcherSort
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , takeMVar
    )
import Control.Exception
    ( SomeException
    , throwIO
    , try
    )


-- ============================================================
-- PUBLIC FUNCTIONS
-- ============================================================

-- Sequential version.
batcherSort :: [Int] -> [Int]
batcherSort [] = []
batcherSort values =
    take originalSize sorted
  where
    originalSize = length values
    targetSize = nextPowerOfTwo originalSize

    padded =
        values
            ++ replicate
                (targetSize - originalSize)
                maxBound

    sorted = oddEvenMergeSort padded


-- Parallel version.
-- k = maximum number of simultaneously used threads.
parallelBatcherSort :: Int -> [Int] -> IO [Int]
parallelBatcherSort _ [] =
    return []

parallelBatcherSort k values = do
    -- Main thread counts as one thread,
    -- so only k - 1 additional workers may be created.
    workerSlots <- newMVar (max 0 (k - 1))

    let originalSize = length values
        targetSize = nextPowerOfTwo originalSize

        padded =
            values
                ++ replicate
                    (targetSize - originalSize)
                    maxBound

    sorted <-
        parallelOddEvenMergeSort
            workerSlots
            padded

    return (take originalSize sorted)


-- ============================================================
-- PARALLEL BATCHER SORT
-- ============================================================

parallelOddEvenMergeSort
    :: MVar Int
    -> [Int]
    -> IO [Int]

parallelOddEvenMergeSort _ [] =
    return []

parallelOddEvenMergeSort _ [value] =
    return [value]

parallelOddEvenMergeSort workerSlots values = do
    let middle = length values `div` 2
        (left, right) = splitAt middle values

    (sortedLeft, sortedRight) <-
        parallelPair
            workerSlots
            (parallelOddEvenMergeSort workerSlots left)
            (parallelOddEvenMergeSort workerSlots right)

    parallelOddEvenMerge
        workerSlots
        sortedLeft
        sortedRight


parallelOddEvenMerge
    :: MVar Int
    -> [Int]
    -> [Int]
    -> IO [Int]

parallelOddEvenMerge _ [x] [y] =
    return (compareExchange x y)

parallelOddEvenMerge workerSlots left right = do
    let (leftEven, leftOdd) =
            splitEvenOdd left

        (rightEven, rightOdd) =
            splitEvenOdd right

    (mergedEven, mergedOdd) <-
        parallelPair
            workerSlots
            (parallelOddEvenMerge
                workerSlots
                leftEven
                rightEven)
            (parallelOddEvenMerge
                workerSlots
                leftOdd
                rightOdd)

    return $
        compareOddPairs $
            interleave mergedEven mergedOdd


-- ============================================================
-- THREAD CONTROL
-- ============================================================

-- Execute two independent computations.
-- If a worker slot is available, the first computation
-- runs in a separate thread.
parallelPair
    :: MVar Int
    -> IO [Int]
    -> IO [Int]
    -> IO ([Int], [Int])

parallelPair workerSlots firstAction secondAction = do
    canCreateThread <- reserveWorker workerSlots

    if canCreateThread
        then do
            resultVar <-
                newEmptyMVar
                    :: IO (MVar (Either SomeException [Int]))

            _ <- forkIO $ do
                result <- try firstAction

                putMVar resultVar result

                releaseWorker workerSlots

            secondResult <- secondAction

            firstResultOrError <-
                takeMVar resultVar

            firstResult <-
                either throwIO return firstResultOrError

            return
                ( firstResult
                , secondResult
                )

        else do
            firstResult <- firstAction
            secondResult <- secondAction

            return
                ( firstResult
                , secondResult
                )


-- Try to reserve one additional worker.
-- This operation does NOT block.
reserveWorker :: MVar Int -> IO Bool
reserveWorker workerSlots =
    modifyMVar workerSlots $ \available ->
        if available > 0
            then
                return
                    ( available - 1
                    , True
                    )
            else
                return
                    ( available
                    , False
                    )


releaseWorker :: MVar Int -> IO ()
releaseWorker workerSlots =
    modifyMVar_ workerSlots $ \available ->
        return (available + 1)


-- ============================================================
-- SEQUENTIAL BATCHER SORT
-- ============================================================

oddEvenMergeSort :: [Int] -> [Int]
oddEvenMergeSort [] = []
oddEvenMergeSort [value] = [value]

oddEvenMergeSort values =
    oddEvenMerge
        (oddEvenMergeSort left)
        (oddEvenMergeSort right)
  where
    middle = length values `div` 2
    (left, right) =
        splitAt middle values


oddEvenMerge :: [Int] -> [Int] -> [Int]
oddEvenMerge [x] [y] =
    compareExchange x y

oddEvenMerge left right =
    compareOddPairs merged
  where
    (leftEven, leftOdd) =
        splitEvenOdd left

    (rightEven, rightOdd) =
        splitEvenOdd right

    mergedEven =
        oddEvenMerge leftEven rightEven

    mergedOdd =
        oddEvenMerge leftOdd rightOdd

    merged =
        interleave mergedEven mergedOdd


-- ============================================================
-- HELPERS
-- ============================================================

compareExchange :: Int -> Int -> [Int]
compareExchange x y
    | x <= y = [x, y]
    | otherwise = [y, x]


splitEvenOdd :: [a] -> ([a], [a])
splitEvenOdd [] =
    ([], [])

splitEvenOdd [x] =
    ([x], [])

splitEvenOdd (x : y : rest) =
    ( x : evenValues
    , y : oddValues
    )
  where
    (evenValues, oddValues) =
        splitEvenOdd rest


interleave :: [a] -> [a] -> [a]
interleave [] ys =
    ys

interleave xs [] =
    xs

interleave (x : xs) (y : ys) =
    x : y : interleave xs ys


compareOddPairs :: [Int] -> [Int]
compareOddPairs [] =
    []

compareOddPairs (x : xs) =
    x : compareRest xs
  where
    compareRest (a : b : rest) =
        min a b
            : max a b
            : compareRest rest

    compareRest rest =
        rest


nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n =
    go 1
  where
    go power
        | power >= n = power
        | otherwise = go (power * 2)