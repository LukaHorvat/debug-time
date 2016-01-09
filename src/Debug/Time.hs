-- | Here's how you can use this package
--
-- @
-- module Main where
--
-- import Debug.Time
--
-- -- | Naive implementation
-- fibs :: [Int]
-- fibs = map (fibAt . maybeTimer) [1..]
--     where maybeTimer 10 = startTimer "10-30" 10
--           maybeTimer 30 = traceTimer "10-30" 30
--           maybeTimer 20 = startTimer "20-40" 20
--           maybeTimer 40 = traceTimer "20-40" 40
--           maybeTimer n  = n
--
-- fibAt :: Int -> Int
-- fibAt 1 = 1
-- fibAt 2 = 1
-- fibAt n = fibAt (n - 1) + fibAt (n - 2)
--
-- main :: IO ()
-- main = do
--     initializeTimers
--     putStrLn "Calculating the first 40 fibonacci numbers while tracing the time elapsed between"
--     putStrLn "the computations 10-30 and 20-40"
--     mapM_ print (take 40 fibs)
-- @
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module Debug.Time (startTimer, traceTimer, restartTimer, initializeTimers) where

import Data.Map (Map)
import qualified Data.Map as Map
import System.Clock (TimeSpec, timeSpecAsNanoSecs, getTime, diffTimeSpec, Clock(Monotonic))
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef)
import qualified Data.IORef as Ref
import Debug.Trace (trace)
import Control.Monad (void)
import Numeric

{-# NOINLINE timers #-}
timers :: IORef (Map String TimeSpec)
timers = unsafePerformIO $! Ref.newIORef Map.empty

{-# NOINLINE readTimer #-}
readTimer :: String -> Integer
readTimer name = unsafePerformIO $ do
    now  <- getTime Monotonic
    time <- Map.lookup name <$> Ref.readIORef timers
    case time of
        Nothing -> error $ "Attempting to read the timer '" ++ name ++ "' that has not been started"
        Just t  -> return $! timeSpecAsNanoSecs (diffTimeSpec now t)

-- | Initializes the timer store. This makes the first measurement more reliable.
initializeTimers :: IO ()
initializeTimers = void $! Ref.readIORef timers

-- | Ties the evaluation of the value with the start of a timer with the given name.
startTimer :: String -> a -> a
startTimer name x = start `seq` x where
    start = unsafePerformIO $ do
        time <- getTime Monotonic
        Ref.modifyIORef' timers (Map.insert name time)

-- | Ties the evaluation of the value with an action tracing the elapsed time
--   since the start of the timer.
traceTimer :: String -> a -> a
traceTimer name = trace (format (readTimer name))
    where format time = "Timer " ++ name ++ ": " ++ asSeconds time ++ " seconds."
          asSeconds time = showFFloat Nothing (fromIntegral time / 1000000000 :: Double) ""

-- | Synonym for startTimer.
restartTimer :: String -> a -> a
restartTimer = startTimer
