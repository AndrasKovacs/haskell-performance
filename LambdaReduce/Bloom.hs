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

Of course this isn't an API we'd like to present. Ideally 'Bloom' filters should
not be visible at all to programmers.
-}

-- TODO: benchmark this in comparison to hereditary subst without
--       Bloom filters

{-# language
  GeneralizedNewtypeDeriving, PatternSynonyms, BangPatterns,
  LambdaCase, FlexibleInstances, FlexibleContexts #-}

-- OPTIMIZATIONS:
-- reduce usage of bloom in subst/rename
-- try to unbox state monad in hsubst

module Bloom where

import Data.Bifunctor
import Data.Bits
import Debug.Trace

-- TODO: hybrid data structure that uses an association vector when small
-- and switches to IntMap when large
import qualified Data.HashMap.Strict as HM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import qualified Frontend as F
import Control.Monad.State.Strict

type Name = Int

-- Note: we don't make any static distinction between normal
-- and reducible terms
data Term
  = Var !Name
  | App {-# unpack #-} !Bloom !Term !Term
  | Lam {-# unpack #-} !Bloom !Name !Term
  deriving Show

fromRaw :: F.RawTerm -> (Term, Name)
fromRaw t = runState (go HM.empty t) (0 :: Name) where
  go m (F.Var i)   = pure $ Var $ m HM.! i
  go m (F.App f x) = app <$> go m f <*> go m x
  go m (F.Lam i t) = do
    i' <- get <* modify (+1)
    lam i' <$> go (HM.insert i i' m) t

toRaw :: Term -> F.RawTerm
toRaw = go HM.empty names where

  names = do {x <- [1..]; replicateM x $ ['a'..'z'] ++ ['A'..'Z']}

  go m ns     (Var i)     = F.Var (m HM.! i)
  go m ns     (App _ f x) = F.App (go m ns f) (go m ns x)
  go m (n:ns) (Lam _ i t) = F.Lam n (go (HM.insert i n m) ns t)


app :: Term -> Term -> Term
app f x = App (bloom f .|. bloom x) f x
{-# inline app #-}

lam :: Name -> Term -> Term
lam i t = Lam (bloom t) i t
{-# inline lam #-}


-- Bloom filter
--------------------------------------------------------------------------------

mask :: Name
mask = finiteBitSize (undefined :: Name) - 1
{-# inline mask #-}

-- What data structure would be nice for checking Miller pattern condition?
-- I. e. that some vars are the *only* free variables in a term?
newtype Bloom = Bloom Name deriving (Eq, Ord, Bits, Num)

instance Show Bloom where
  show b = show $ filter (`hasVar` b) [0..mask]

addVar :: Name -> Bloom -> Bloom
addVar i b = b .|. unsafeShiftL 1 (i .&. mask)
{-# inline addVar #-}

hasVar :: Name -> Bloom -> Bool
hasVar i b = (b .&. unsafeShiftL 1 (i .&. mask)) /= 0
{-# inline hasVar #-}

bloom :: Term -> Bloom
bloom (Var i)     = addVar i 0
bloom (App b _ _) = b
bloom (Lam b _ _) = b


-- Substitution
--------------------------------------------------------------------------------

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
hsubst env !i !sub = \case
  v@(Var i')
    | i == i'   -> pure sub
    | otherwise -> pure v
  a@(App b f x)
    | hasVar i b -> hsubst env i sub f >>= \case
        Lam b' i' t'
          | hasVar i' b' -> do
              x' <- hsubst env i sub x
              hsubst env i' x' t'
          | otherwise -> pure t'
        f' -> app f' <$> hsubst env i sub x
    | otherwise -> pure a
  l@(Lam _ i' t)
    | i /= i' && hasVar i' (bloom sub) && IS.member i' env -> do
        i'' <- get <* modify (+1)
        hsubst (IS.insert i'' env) i sub (rename i' i'' t)
    | otherwise -> lam i' <$> hsubst (IS.insert i' env) i sub t

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

-- Note: we're deliberately lazy in "env"
-- because it's possible we never have to compute it

-- the only place where we possibly need "env" is when we
-- check for capute in the "Lam" case.

-- unfortunately, we can't be lazy in "t'" since we need to update the
-- "Name" state correctly.

nf0 :: (Term, Name) -> Term
nf0 (t, n) = evalState (nf IS.empty t) n

rawNf :: String -> Either String F.RawTerm
rawNf = fmap (toRaw . nf0 . fromRaw) . F.parse


-- Testing
--------------------------------------------------------------------------------


-- Debugging versions

-- hsubst :: IntSet -> Name -> Term -> Term -> State Name Term
-- hsubst env !i !sub t = do
--   res <- case t of
--     v@(Var i')
--       | i == i'   -> pure sub
--       | otherwise -> pure v
--     a@(App b f x)
--       | hasVar i b -> hsubst env i sub f >>= \case
--           Lam b' i' t'
--             | hasVar i' b' -> do
--                 x' <- hsubst env i sub x
--                 hsubst env i' x' t'
--             | otherwise -> pure t'
--           f' -> app f' <$> hsubst env i sub x
--       | otherwise -> pure a
--     l@(Lam _ i' t)
--       | i /= i' && hasVar i' (bloom sub) && IS.member i' env -> do
--           i'' <- get <* modify (+1)
--           hsubst (IS.insert i'' env) i sub (rename i' i'' t)
--       | otherwise -> lam i' <$> hsubst (IS.insert i' env) i sub t
--   traceM ""
--   traceM ("subst result: " ++ pretty res)
--   traceM ("subst var: " ++ show i ++ " term: " ++ pretty sub ++ " into: " ++ pretty t)
--   pure res

-- nf :: IntSet -> Term -> State Name Term
-- nf env t = do
--   res <- case t of
--     v@(Var i)   -> pure v
--     (App _ f x) -> nf env f >>= \case
--       Lam b i t
--         | hasVar i b -> do
--             x' <- nf env x
--             hsubst env i x' t
--         | otherwise -> pure t
--       f' -> app f' <$> nf env x
--     (Lam b i t) -> lam i <$> nf (IS.insert i env) t
--   traceM ""
--   traceM ("nf  : " ++ pretty res)
--   traceM ("term: " ++ pretty t)
--   pure res


