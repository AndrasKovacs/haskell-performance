{-# language
  GADTs, BangPatterns, TypeFamilies, MagicHash, KindSignatures,
  PatternSynonyms, ViewPatterns, RoleAnnotations, LambdaCase #-}

module Main where

import GHC.Prim
import Criterion.Main
import Control.Monad
import Control.Applicative

type role FMaybe representational
data FMaybe (a :: *) = FM !Int Any

pattern FNothing <- (FM 0 _) where
  FNothing = FM 0 undefined

pattern FJust a <- (FM 1 (unsafeCoerce# -> a)) where
  FJust a = FM 1 (unsafeCoerce# a)

fmaybe :: b -> (a -> b) -> FMaybe a -> b
fmaybe n j = \case FJust a -> j a; _ -> n

instance Functor FMaybe  where
  fmap f (FJust a) = FJust (f a)
  fmap f x         = unsafeCoerce# x
  {-# inline fmap #-}

instance Applicative FMaybe where
  pure  = return
  (<*>) = ap

instance Monad FMaybe where
  return = FJust
  FJust a >>= f = f a
  x       >>= f = unsafeCoerce# x
  {-# inline return #-}
  {-# inline (>>=) #-}

instance Show a => Show (FMaybe a) where
  showsPrec n = showsPrec n . fmaybe Nothing Just

instance Alternative FMaybe where
  empty = FNothing
  {-# inline empty #-}
  FJust a <|> _ = FJust a
  _       <|> b = b
  {-# inline (<|>) #-}

instance MonadPlus FMaybe where
  mzero = empty
  {-# inline mzero #-}
  mplus = (<|>)
  {-# inline mplus #-}

msum1 :: [FMaybe ()]
msum1 = replicate 10000 FNothing ++ [FJust ()]

msum2 :: [Maybe ()]
msum2 = replicate 10000 Nothing ++ [Just ()]

main = defaultMain [
  -- bench "flat msum" $ whnf msum msum1,
  -- bench "msum" $ whnf msum msum2,
  bench "sequence flat" $ whnf sequence (replicate 100000 (FJust ())),
  bench "sequence" $ whnf sequence (replicate 100000 (Just ()))
  ]
