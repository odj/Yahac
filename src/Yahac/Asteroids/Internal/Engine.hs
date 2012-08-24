module Yahac.Asteroids.Internal.Engine (
    setInterval
) where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

-- | A timed thread for performing physics
setInterval :: IO () -> Integer -> IO (MVar Bool)
setInterval f timeout = do 
    stopMV <- newEmptyMVar
    let t = fromIntegral $ timeout * 1000
        driver = do
            continue <- isEmptyMVar stopMV
            case continue of
                True -> threadDelay t >> f >> driver
                False -> do
                    id <- myThreadId
                    putStrLn $ "Stopping interval thread: " ++ show id
        
    forkIO driver
    return stopMV

        

