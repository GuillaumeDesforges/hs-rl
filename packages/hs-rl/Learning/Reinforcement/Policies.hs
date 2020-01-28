{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Learning.Reinforcement.Policies where

import           Data.List.Extras.Argmax        ( argmax )

import           System.Random

import           Learning.Reinforcement.Types
import           Learning.Reinforcement.Utils

greedyPolicy
    :: ( Env e s a m
       , StateActionModel e s a
       , StateActionValueFunc f s a
       , Enum a
       , Bounded a
       )
    => e
    -> f
    -> s
    -> m a
greedyPolicy env qfunc state = do
    let allowedActions = filter (isAllowed env state) [minBound .. maxBound]
        greedyAction =
            argmax (\action -> evaluateSA qfunc state action) allowedActions
    return greedyAction

epsilonGreedyPolicy
    :: forall e s a f
     . ( Env e s a IO
       , StateActionModel e s a
       , StateActionValueFunc f s a
       , Enum a
       , Bounded a
       )
    => Float
    -> e
    -> f
    -> s
    -> IO a
epsilonGreedyPolicy epsilon env qfunc state = do
    explore <- (randomRIO (0, 1) :: IO Float)
    if explore < epsilon
        then do
            let allowedActions =
                    filter (isAllowed env state) ([minBound .. maxBound] :: [a])
            action <- randomChoice allowedActions
            return action
        else greedyPolicy env qfunc state
