{-|
Normalization by evaluation, with de Bruijn levels. Call-by-need.
-}

{-# language BangPatterns #-}

module NBE where

import Data.Maybe
import qualified Data.HashMap.Strict as HM

import qualified Frontend as F

-- | de Bruijn levels
data Term
  = Var !Int
  | App !Term !Term
  | Lam !Term
  deriving (Show)

data Val
  = VVar !Int
  | VApp !Val !Val
  | VLam (Val -> Val)

fromRaw :: F.RawTerm -> Term
fromRaw = go HM.empty 0 where
  go m !d (F.Var i)   = Var (m HM.! i)
  go m  d (F.App f x) = App (go m d f) (go m d x)
  go m  d (F.Lam i t) = Lam (go (HM.insert i d m) (d + 1) t)

vapp :: Val -> Val -> Val
vapp (VLam f) x = f x
vapp f        x = VApp f x

-- We could use alternative structures for "env"
evalList :: Term -> Val
evalList = go [] 0 where
  go env !d (Var i)   = env !! (d - i - 1)
  go env  d (App f x) = vapp (go env d f) (go env d x)
  go env  d (Lam t)   = VLam $ \t' -> go (t':env) (d + 1) t

quote :: Val -> Term
quote = go 0 where
  go !d (VVar i)   = Var i
  go  d (VApp f x) = App (go d f) (go d x)
  go  d (VLam f)   = Lam (go (d + 1) (f (VVar d)))

nfList :: Term -> Term
nfList = quote . evalList

test = unlines [
  "let z = lam s z.z in",
  "let s = lam n s z. s (n s z) in",
  "let plus = lam a b s z. a s (b s z) in",
  "let mult = lam a b s z. a (b s) z in",
  "mult (s (s z)) (s (s z))"
  ]


