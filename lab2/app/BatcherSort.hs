module BatcherSort
    ( batcherSort
    , parallelBatcherSort
    , parallelBatcherSortDebug
    ) where
import Control.Concurrent
    ( forkIO
    , myThreadId
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , takeMVar
    , withMVar
    )
import Control.Exception
    ( SomeException
    , throwIO
    , try
    )

    
parallelThreshold :: Int
parallelThreshold = 1024

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
parallelBatcherSort =
    parallelBatcherSortInternal False


parallelBatcherSortDebug :: Int -> [Int] -> IO [Int]
parallelBatcherSortDebug =
    parallelBatcherSortInternal True


parallelBatcherSortInternal :: Bool -> Int -> [Int] -> IO [Int]
parallelBatcherSortInternal _ _ [] =
    return []

parallelBatcherSortInternal debug k values = do
    workerSlots <- newMVar (max 0 (k - 1))

    -- Prevent debug messages from different threads
    -- from being mixed together.
    logLock <- newMVar ()

    let originalSize = length values
        targetSize = nextPowerOfTwo originalSize

        padded =
            values
                ++ replicate
                    (targetSize - originalSize)
                    maxBound

    sorted <-
        parallelOddEvenMergeSort
            debug
            logLock
            workerSlots
            padded

    return (take originalSize sorted)


parallelOddEvenMergeSort
    :: Bool
    -> MVar ()
    -> MVar Int
    -> [Int]
    -> IO [Int]

parallelOddEvenMergeSort _ _ _ [] =
    return []

parallelOddEvenMergeSort _ _ _ [value] =
    return [value]

parallelOddEvenMergeSort debug logLock workerSlots values
    | length values <= parallelThreshold =
        return (oddEvenMergeSort values)

    | otherwise = do
        let middle = length values `div` 2
            (left, right) = splitAt middle values

            description =
                "sorting "
                    ++ show (length values)
                    ++ " elements"

        (sortedLeft, sortedRight) <-
            parallelPair
                debug
                logLock
                workerSlots
                description
                (parallelOddEvenMergeSort
                    debug
                    logLock
                    workerSlots
                    left)
                (parallelOddEvenMergeSort
                    debug
                    logLock
                    workerSlots
                    right)

        parallelOddEvenMerge
            debug
            logLock
            workerSlots
            sortedLeft
            sortedRight


parallelOddEvenMerge
    :: Bool
    -> MVar ()
    -> MVar Int
    -> [Int]
    -> [Int]
    -> IO [Int]

parallelOddEvenMerge _ _ _ [x] [y] =
    return (compareExchange x y)

parallelOddEvenMerge debug logLock workerSlots left right
    | length left + length right <= parallelThreshold =
        return (oddEvenMerge left right)

    | otherwise = do
        let (leftEven, leftOdd) =
                splitEvenOdd left

            (rightEven, rightOdd) =
                splitEvenOdd right

            description =
                "merging "
                    ++ show (length left)
                    ++ " + "
                    ++ show (length right)
                    ++ " elements"

        (mergedEven, mergedOdd) <-
            parallelPair
                debug
                logLock
                workerSlots
                description
                (parallelOddEvenMerge
                    debug
                    logLock
                    workerSlots
                    leftEven
                    rightEven)
                (parallelOddEvenMerge
                    debug
                    logLock
                    workerSlots
                    leftOdd
                    rightOdd)

        return $
            compareOddPairs $
                interleave mergedEven mergedOdd


parallelPair
    :: Bool
    -> MVar ()
    -> MVar Int
    -> String
    -> IO [Int]
    -> IO [Int]
    -> IO ([Int], [Int])

parallelPair
    debug
    logLock
    workerSlots
    description
    firstAction
    secondAction = do

    canCreateThread <- reserveWorker workerSlots

    if canCreateThread
        then do
            resultVar <-
                newEmptyMVar
                    :: IO (MVar (Either SomeException [Int]))

            _ <- forkIO $ do
                logThread
                    debug
                    logLock
                    ("START: " ++ description)

                result <- try firstAction

                logThread
                    debug
                    logLock
                    ("END:   " ++ description)

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


logThread :: Bool -> MVar () -> String -> IO ()
logThread False _ _ =
    return ()

logThread True logLock message =
    withMVar logLock $ \_ -> do
        threadId <- myThreadId

        putStrLn $
            "["
                ++ show threadId
                ++ "] "
                ++ message


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