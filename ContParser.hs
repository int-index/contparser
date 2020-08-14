{-# LANGUAGE RankNTypes #-}

module ContParser where

import Control.Monad

newtype P s e a =
  MkP { unP :: forall r. (a -> s -> Either e r) ->
                               s -> Either e r }

instance Functor (P s e) where
  fmap = liftM

instance Applicative (P s e) where
  pure a =
    MkP $ \cont ->
      cont a
  MkP f <*> MkP v =
    MkP $ \cont ->
      f (\g -> v (cont . g))

instance Monad (P s e) where
  MkP m >>= k =
    MkP $ \cont ->
      m (\x -> unP (k x) cont)

liftP :: (s -> Either e a) -> P s e a
liftP p = MkP $
  \cont lexerState ->
  case p lexerState of
    Left e -> Left e
    Right a -> cont a lexerState

failP :: e -> P s e a
failP e = MkP $ \_ _ -> Left e

runP :: P s e a -> s -> Either e a
runP (MkP p) = p (\a _ -> Right a)
