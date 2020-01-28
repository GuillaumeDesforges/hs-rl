module Learning.Reinforcement.Utils where

import           System.Random                  ( randomRIO )

iterateUntilM :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
iterateUntilM condition step x = do
    shouldStop <- condition x
    if shouldStop then return x else (step x) >>= (iterateUntilM condition step)

randomChoice :: [a] -> IO a
randomChoice elems = do
    let l = max 0 ((length elems) - 1)
    n <- randomRIO (0, l)
    return $ elems !! n
