module DelayIO(
    Delayed(..),
    delayIO
    )
where

import Data.IORef

data Delayed a = Delayed {
      delayGet :: IO a,
      delayPending :: IO Bool
}

delayIO :: IO a -> IO (Delayed a)
delayIO f = do
    r <- newIORef Nothing
    return (Delayed (get r) (pending r))
  where
    get r = do
        ma <- readIORef r
        case ma of
          (Just a) -> return a
          Nothing -> do
            a <- f
            writeIORef r (Just a)
            return a

    pending r = do
        ma <- readIORef r
        case ma of
          (Just a) -> return False
          Nothing -> return True

