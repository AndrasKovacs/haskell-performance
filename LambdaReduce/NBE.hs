{-|
Normalization by evaluation, with de Bruijn levels. Call-by-need.
-}

{-# language BangPatterns #-}

module NBE where

import Control.Monad
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

toRaw :: Term -> F.RawTerm
toRaw = go 0 HM.empty names where

  names = do {x <- [1..]; replicateM x $ ['a'..'z'] ++ ['A'..'Z']}

  go d m ns     (Var i)   = F.Var (m HM.! i)
  go d m ns     (App f x) = F.App (go d m ns f) (go d m ns x)
  go d m (n:ns) (Lam t)   = F.Lam n (go (d + 1) (HM.insert d n m) ns t)

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

rawNf :: String -> Either String F.RawTerm
rawNf = fmap (toRaw . nfList . fromRaw) . F.parse

