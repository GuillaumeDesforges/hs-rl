{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Learning.Reinforcement.Algorithms.Sarsa where

import           Control.Monad.State.Lazy       ( StateT
                                                , evalStateT
                                                , get
                                                , put
                                                , modify
                                                , lift
                                                )
import           Learning.Reinforcement.Types
import           Learning.Reinforcement.Utils

sarsa
    :: forall e s a f m
     . ( Env e s a m
       , StateActionValueFunc f s a
       , StateActionModel e s a
       , State s
       , Bounded a
       , Enum a
       , Monad m
       )
    => (e -> f -> s -> m a)
    -> e
    -> Float
    -> Float
    -> Int
    -> m s
    -> m a
    -> f
    -> m f
sarsa makePolicy env alpha gamma maxIter stateInitializer actionInitializer qfunc
    = evalStateT episodes 0
  where
    episodes :: StateT Int m f
    episodes = do
        qfunc' <- iterateUntilM isMaxIter episode qfunc
        return qfunc'

    isMaxIter :: f -> StateT Int m Bool
    isMaxIter qfunc = do
        count <- get
        return $ count >= maxIter

    episode :: f -> StateT Int m f
    episode qfunc = do
        modify (+ 1)
        initialState  <- lift stateInitializer
        initialAction <- lift actionInitializer
        qfunc'        <- lift $ evalStateT
            (iterateUntilM isTerminalState episodeStep qfunc)
            (initialState, initialAction)
        return qfunc'

    isTerminalState :: f -> StateT (s, a) m Bool
    isTerminalState step = do
        (state, _) <- get
        return $ isTerminal state

    episodeStep :: f -> StateT (s, a) m f
    episodeStep qfunc = do
        (state, action) <- get
        let policy = makePolicy env qfunc
        (reward, state') <- lift
            $ Learning.Reinforcement.Types.interact env state action
        action' <- lift $ policy state'
        let qfunc' = updateSAV qfunc state action
                $ sarsaSAVFuncUpdater qfunc state action reward state' action'
        put (state', action')
        return qfunc'

    sarsaSAVFuncUpdater :: f -> s -> a -> Float -> s -> a -> Float
    sarsaSAVFuncUpdater qfunc state action reward state' action' =
        let qsa  = evaluateSA qfunc state action
            qsa' = evaluateSA qfunc state' action'
        in  qsa + alpha * (reward + gamma * qsa' - qsa)
