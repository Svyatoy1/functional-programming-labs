module BatcherSort
    ( batcherSort
    ) where


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


-- Returns the smallest power of two
-- greater than or equal to n.
nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n =
    go 1
  where
    go power
        | power >= n = power
        | otherwise = go (power * 2)


-- Batcher odd-even merge sort.
oddEvenMergeSort :: [Int] -> [Int]
oddEvenMergeSort [] = []
oddEvenMergeSort [value] = [value]
oddEvenMergeSort values =
    oddEvenMerge
        (oddEvenMergeSort left)
        (oddEvenMergeSort right)
  where
    middle = length values `div` 2
    (left, right) = splitAt middle values


-- Merges two already sorted sequences
-- using Batcher's odd-even merge.
oddEvenMerge :: [Int] -> [Int] -> [Int]
oddEvenMerge [x] [y] =
    compareExchange x y

oddEvenMerge left right =
    compareOddPairs merged
  where
    (leftEven, leftOdd) = splitEvenOdd left
    (rightEven, rightOdd) = splitEvenOdd right

    mergedEven =
        oddEvenMerge leftEven rightEven

    mergedOdd =
        oddEvenMerge leftOdd rightOdd

    merged =
        interleave mergedEven mergedOdd


-- Compare-exchange operation.
compareExchange :: Int -> Int -> [Int]
compareExchange x y
    | x <= y = [x, y]
    | otherwise = [y, x]


-- Splits elements by even and odd positions.
splitEvenOdd :: [a] -> ([a], [a])
splitEvenOdd [] = ([], [])
splitEvenOdd [x] = ([x], [])
splitEvenOdd (x : y : rest) =
    (x : evenValues, y : oddValues)
  where
    (evenValues, oddValues) =
        splitEvenOdd rest


-- Interleaves two lists.
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x : xs) (y : ys) =
    x : y : interleave xs ys


-- After odd/even merging, compare elements
-- at positions (1,2), (3,4), (5,6), ...
compareOddPairs :: [Int] -> [Int]
compareOddPairs [] = []
compareOddPairs (x : xs) =
    x : compareRest xs
  where
    compareRest (a : b : rest) =
        min a b
            : max a b
            : compareRest rest

    compareRest rest = rest