{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Learning.Reinforcement.Types where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map



-- State
--   * Can be determined either terminal or non-terminal
class State s where
    isTerminal :: s -> Bool



-- State-Action Value Function
--   * Allows to get a Value from a (State, Action) pair, usually the expected gain G_t
--   * The value of pair (State, Action) can be updated
class StateActionValueFunc f s a where
    evaluateSA :: f -> s -> a -> Float
    updateSAV :: f -> s -> a -> Float -> f

-- Q function as a Map
type QFuncMap s a = Map (s, a) Float
instance (Ord s, Ord a) => StateActionValueFunc (QFuncMap s a) s a where
    evaluateSA q s a = Map.findWithDefault 0 (s, a) q
    updateSAV q s a v = Map.insert (s, a) v q



-- Environment
--   * Gives the next state following an action from a current state
class (Monad m) => Env e s a m where
    interact :: e -> s -> a -> m (Float, s)



-- State-Action model
--   * Defines whether an action is possible or not from a state given an environment
class StateActionModel e s a where
    isAllowed :: e -> s -> a -> Bool

