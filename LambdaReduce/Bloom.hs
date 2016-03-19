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

This should also let us recover some benefits of call-by-need, since we can avoid
reducing arguments of consant functions, if constantness is indicated by the
Bloom filter.
-}

{-# language
  GeneralizedNewtypeDeriving, PatternSynonyms, BangPatterns,
  LambdaCase, CPP, FlexibleInstances, FlexibleContexts #-}

module Bloom where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Bool

-- TODO: hybrid data structure that uses an association vector when small
-- and switches to IntMap when large
import Data.IntMap.Strict ((!), IntMap)
import qualified Data.IntMap as IM
import qualified Data.HashMap.Strict as HM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import qualified Frontend as F
import Control.Monad.State.Strict

type Name = Int

#include "MachDeps.h"
#if SIZEOF_HSINT == 8
mask = 63 :: Name
#elif SIZEOF_HSINT == 4
mask = 31 :: Name
#endif
{-# inline mask #-}

-- What data structure would be nice for checking Miller pattern condition?
-- I. e. that some vars are the *only* free variables in a term?
newtype Bloom = Bloom Name deriving (Eq, Ord, Bits, Num)

instance Show Bloom where
  show b = show $ filter (`hasVar` b) [0..mask]

addVar :: Name -> Bloom -> Bloom
addVar i b = setBit b (i .&. mask)
{-# inline addVar #-}

hasVar :: Name -> Bloom -> Bool
hasVar i b = testBit b (i .&. mask)
{-# inline hasVar #-}

-- Note: we don't make any static distinction between normal
-- and non-normal terms
data Term
  = Var !Name
  | App {-# unpack #-} !Bloom !Term !Term
  | Lam {-# unpack #-} !Bloom !Name !Term
  deriving Show

bloom :: Term -> Bloom
bloom (Var i)     = addVar i 0
bloom (App b _ _) = b
bloom (Lam b _ _) = b

-- | Smart constructors
app :: Term -> Term -> Term
app f x = App (bloom f .|. bloom x) f x
{-# inline app #-}

lam :: Name -> Term -> Term
lam i t = Lam (bloom t) i t
{-# inline lam #-}

fromRaw :: F.RawTerm -> (Term, Name)
fromRaw t = runState (go HM.empty t) (0 :: Name) where
  go m (F.Var i)   = pure $ Var $ m HM.! i
  go m (F.App f x) = app <$> go m f <*> go m x
  go m (F.Lam i t) = do
    i' <- get <* modify (+1)
    lam i' <$> go (HM.insert i i' m) t

-- | Rename a free variable
rename :: Name -> Name -> Term -> Term
rename i i' = go where
  go v@(Var i'')
    | i'' == i  = Var i'
    | otherwise = v
  go a@(App b f x)
    | hasVar i b = app (go f) (go x)
    | otherwise  = a
  go l@(Lam b i'' t)
    | i'' /= i && hasVar i b = lam i'' (go t)
    | otherwise = l

-- Note: GHC doesn't unbox the state tuple (as seen in Core!)
-- And if we unbox it outselves, it still doesn't unbox the Int state!

-- | Substitute a normal term into a normal term, yielding a normal term.
--   State contains a fresh name.
hsubst :: IntSet -> Name -> Term -> Term -> State Name Term
hsubst env !i t' = \case
  v@(Var i')
    | i == i'   -> pure t'
    | otherwise -> pure v
  a@(App b f x)
    | hasVar i b -> hsubst env i t' f >>= \case
        Lam b' i' t'
          | hasVar i' b' -> do
              x' <- hsubst env i t' x
              hsubst env i' x' t'
          | otherwise -> pure t'
        f' -> app f' <$> hsubst env i t' x
    | otherwise -> pure a
  l@(Lam _ i' t)
    | i /= i' && hasVar i' (bloom t') && IS.member i' env -> do
        i'' <- get <* modify (+1)
        hsubst (IS.insert i'' env) i t' (rename i' i'' t)
    | otherwise -> pure l

-- Note: we're deliberately lazy in "env" and "t'",
-- because possibly we never have to compute them

-- | Reduce to normal form.
nf :: IntSet -> Term -> State Name Term
nf env v@(Var i)   = pure v
nf env (App _ f x) = nf env f >>= \case
  Lam b i t
    | hasVar i b -> do
        x' <- nf env x
        hsubst env i x' t
    | otherwise -> pure t
  f' -> app f' <$> nf env x
nf env (Lam b i t) = lam i <$> nf (IS.insert i env) t

nf0 :: (Term, Name) -> Term
nf0 (t, n) = evalState (nf IS.empty t) n

