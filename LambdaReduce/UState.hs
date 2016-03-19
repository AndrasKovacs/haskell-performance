{-# language UnboxedTuples, MagicHash, BangPatterns #-}

module UState where

newtype State s a = State {unState :: s -> (# a, s #)}

instance Functor (State s) where
  fmap f (State ma) = State $ \s -> let !ss = s in case ma ss of (# !a, !s #) -> (# f a, s #)
  {-# inline fmap #-}

instance Applicative (State s) where
  pure a = State $ \s -> let !ss = s in (# a, s #)
  {-# inline pure #-}
  State mf <*> State ma = State $ \s -> let !ss = s in case mf ss of
    (# !f , !s' #) -> case ma s' of
      (# !a, !s'' #) -> let !fa = f a in (# fa, s'' #)
  {-# inline (<*>) #-}

instance Monad (State s) where
  return = pure
  {-# inline return #-}
  State ma >>= f = State $ \s -> let !ss = s in case ma ss of
    (# !a, !s' #) -> unState (f a) s'
  {-# inline (>>=) #-}

get :: State s s
get = State $ \s -> let !ss = s in (# ss, ss #)
{-# inline get #-}

put :: s -> State s ()
put !s = State $ \s -> let !ss = s in (# (), ss #)
{-# inline put #-}

modify :: (s -> s) -> State s ()
modify f = State $ \s -> let !fs = f s in (# (), fs #)
{-# inline modify #-}

evalState :: State s a -> s -> a
evalState (State ma) !s = case ma s of (# !a, !_ #) -> a
{-# inline evalState #-}

execState :: State s a -> s -> s
execState (State ma) !s = case ma s of (# !_, !s #) -> s
{-# inline execState #-}

runState :: State s a -> s -> (a, s)
runState (State ma) !s = case ma s of (# !a, !s #) -> (a, s)
{-# inline runState #-}

