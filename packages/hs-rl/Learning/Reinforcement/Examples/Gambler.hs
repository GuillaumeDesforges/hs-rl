{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Learning.Reinforcement.Examples.Gambler where


-- import Control.Monad.Loops

import           Control.Monad.State.Lazy       ( forM_ )

import qualified Data.Map                      as Map
import           Data.List.Extras.Argmax        ( argmax )

import           System.Random

import           Learning.Reinforcement.Types
import           Learning.Reinforcement.Policies
import           Learning.Reinforcement.Algorithms.Sarsa

-- -- GAMBLER EXAMPLE

-- State
type GamblerState = Int
instance State GamblerState where
    isTerminal state = state <= 0 || state >= 100

-- Action
data GamblerAction = GamblerAction Int deriving (Ord, Eq, Show)
instance Bounded GamblerAction where
    minBound = GamblerAction 1
    maxBound = GamblerAction 99
instance Enum GamblerAction where
    toEnum x = GamblerAction x
    fromEnum (GamblerAction x) = x

-- Q
type GamblerQFunc = QFuncMap GamblerState GamblerAction

-- Env (empty in this case)
data GamblerEnv = GamblerEnv

-- Glue all together
instance Env GamblerEnv GamblerState GamblerAction IO where
    interact _ state (GamblerAction action) = do
        r <- (randomRIO (0, 1) :: IO Int)
        let state' = if r == 1 then state + action else state - action
            reward = if state' >= 100 then 1 else 0
        return (reward, state')

instance StateActionModel GamblerEnv GamblerState GamblerAction where
    isAllowed _ state (GamblerAction action) =
        0 <= action && action <= (min state (100 - state))


----------
-- MAIN --
----------

main :: IO ()
main = do
    qfunc <- sarsa (epsilonGreedyPolicy 0.4)
                   env
                   alpha
                   gamma
                   maxIter
                   stateInitializer
                   actionInitializer
                   qfuncInit

    forM_ ([1 .. 99] :: [GamblerState]) (displayBestAction qfunc)

  where
    env               = GamblerEnv
    alpha             = 0.1
    gamma             = 0.1
    maxIter           = 100000
    stateInitializer  = return (1 :: GamblerState)
    actionInitializer = return $ GamblerAction 1
    qfuncInit         = Map.empty :: GamblerQFunc

    displayBestAction :: GamblerQFunc -> GamblerState -> IO ()
    displayBestAction qfunc state = do
        let bestAction = argmax (\action -> evaluateSA qfunc state action)
                                ([minBound .. maxBound] :: [GamblerAction])
        putStr $ show state
        putStr " -> "
        putStr $ show bestAction
        putStr "\n"
