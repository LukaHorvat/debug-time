module Main where

import Debug.Time

-- | Naive implementation
fibs :: [Int]
fibs = map (fibAt . maybeTimer) [1..]
    where maybeTimer 10 = startTimer "10-30" 10
          maybeTimer 30 = traceTimer "10-30" 30
          maybeTimer 20 = startTimer "20-40" 20
          maybeTimer 40 = traceTimer "20-40" 40
          maybeTimer n  = n

fibAt :: Int -> Int
fibAt 1 = 1
fibAt 2 = 1
fibAt n = fibAt (n - 1) + fibAt (n - 2)

main :: IO ()
main = do
    initializeTimers
    putStrLn "Calculating the first 40 fibonacci numbers while tracing the time elapsed between"
    putStrLn "the computations 10-30 and 20-40"
    mapM_ print (take 40 fibs)
