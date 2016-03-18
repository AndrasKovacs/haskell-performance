{-# language
  GeneralizedNewtypeDeriving, PatternSynonyms, BangPatterns,
  LambdaCase, CPP #-}

{-|
Cache variable occurrences in Bloom filters.

We use global 'Int'-s for variables, and substitute using the no-shadowing rule.
We have strict call-by-value hereditary substitution.

For more information on the no-shadowing rule, refer to section 3.2 in

http://research.microsoft.com/en-us/um/people/simonpj/Papers/inlining/inline.pdf

In a nutshell, when reducing @(\x.t) t'@, if @x@ occurs in the current context, then we
rename it to a fresh variable inside @t@. Otherwise @x@ cannot occur freely in @t'@,
therefore capture is not possible.

I do not believe that no-shadowing is actually an *invariant*; it's just
that we always eliminate shadowing before substitution. It could be interesting to
think about whether we can really enforce no-shadowing in an efficient manner.

The reason why we cache all variables instead of just free vars is that subtracting
bound vars from caches is expensive (linear in the size of a term below a binding),
and non-shadowing non-de-Bruijn vars hopefully reduce false positives in
free var occurs checks.

For example, in @lam a b c. a@, the top-level cache will have an entry for @a@, but
of course not for the other bindings. We only propagate actually used variables.

-}

module Bloom where

import Data.Bits
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Int

import Debug.Trace

import qualified Frontend as F

-- TODO: hybrid data structure that uses an association vector when small
-- and switches to IntMap when large
import qualified Data.IntMap.Strict as IM

import qualified Data.HashMap.Strict as HM

type Name = Int

#include "MachDeps.h"
#if SIZEOF_HSINT == 8
mask = 63 :: Name
#elif SIZEOF_HSINT == 4
mask = 31 :: Name
#endif

-- What data structure would be nice for checking Miller pattern condition?
-- I. e. that some vars are the *only* free variables in a term?
newtype Bloom = Bloom Name deriving (Eq, Ord, Bits, Num)

instance Show Bloom where
  show b = show $ filter (`hasVar` b) [0..mask]

addVar :: Name -> Bloom -> Bloom
addVar i b = setBit b (i .&. mask)

hasVar :: Name -> Bloom -> Bool
hasVar i b = testBit b (i .&. mask)

hasFV :: Name -> Term -> Bool
hasFV i (Var i')     = i == i'
hasFV i (App b f x)  = hasVar i b && (hasFV i f || hasFV i x)
hasFV i (Lam b i' t) = hasVar i b && i /= i' && hasFV i t

data Term =
    Var !Name
  | App !Bloom !Term !Term
  | Lam !Bloom !Name !Term
  deriving (Show)

-- | Approximate vars
vars :: Term -> Bloom
vars (Var i)     = addVar i 0
vars (App b _ _) = b
vars (Lam b _ _) = b

-- | Smart constructors
app :: Term -> Term -> Term
app t1 t2 = App (vars t1 .|. vars t2) t1 t2

lam :: Name -> Term -> Term
lam i t = Lam (vars t) i t

-- | Also rename uniquely.
fromRaw :: F.RawTerm -> Term
fromRaw t = evalState (go HM.empty t) (0 :: Name) where
  go m (F.Var i)   = pure $ Var $ m HM.! i
  go m (F.App f x) = app <$> go m f <*> go m x
  go m (F.Lam i t) = do
    i' <- get <* modify (+1)
    lam i' <$> go (HM.insert i i' m) t

