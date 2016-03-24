{-|
Normalization by evaluation, with de Bruijn levels. Call-by-need.
-}

{-# language BangPatterns, DataKinds, GADTs, TypeFamilies #-}

module NBE where

import Control.Monad
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Control.DeepSeq

import qualified Data.IntMap as IM
import qualified Data.Vector as V

import qualified Frontend as F

-- I'm using GADTs because I'm paranoid about extra copying
-- The ADT version seems to be just so far, though

-- data Term = Var !Int | App !Term Term | Lam !Term
-- data Val = VVar !Int | VApp !Val Val | VLam !(Val -> Val)

-- instance NFData Term where
--   rnf (App f x) = rnf f `seq` rnf x
--   rnf (Lam t)   = rnf t
--   rnf (Var _)   = ()

data Tag = Sem | Syn

data Term' (t :: Tag) where
  Var  :: !Int -> Term' t
  App  :: !(Term' t) -> (Term' t) -> Term' t
  Lam  :: !(Term' Syn) -> Term' Syn
  VLam :: !(Term' Sem -> Term' Sem) -> Term' Sem

instance NFData (Term' t) where
  rnf (App f x) = rnf f `seq` rnf x
  rnf (Lam t)   = rnf t
  rnf (VLam f)  = rnf f
  rnf (Var _)   = ()

type Term = Term' Syn
type Val  = Term' Sem

fromRaw :: F.RawTerm -> Term
fromRaw = go HM.empty 0 where
  go m !d (F.Var i)   = Var (m HM.! i)
  go m  d (F.App f x) = App (go m d f) (go m d x)
  go m  d (F.Lam i t) = Lam (go (HM.insert i d m) (d + 1) t)

toRaw :: Term -> F.RawTerm
toRaw = go 0 HM.empty names where

  names = do {x <- [1..]; replicateM x $ ['a'..'z'] ++ ['A'..'Z']}

  go :: Int -> HM.HashMap Int String -> [String] -> Term -> F.RawTerm
  go d m ns     (Var i)   = F.Var (m HM.! i)
  go d m ns     (App f x) = F.App (go d m ns f) (go d m ns x)
  go d m (n:ns) (Lam t)   = F.Lam n (go (d + 1) (HM.insert d n m) ns t)

($$) :: Val -> Val -> Val
($$) (VLam f) x = f x
($$) f        x = App f x
infixl 9 $$
{-# inline ($$) #-}

quote :: Val -> Term
quote = go 0 where
  go :: Int -> Val -> Term
  go !d (Var i)   = Var i
  go  d (VLam f)  = Lam (go (d + 1) (f (Var d)))
  go  d (App f x) = App (go d f) (go d x)
{-# inline quote #-}  

evalList :: Term -> Val
evalList = go [] 0 where
  go env !d (Var i)   = env !! (d - i - 1)
  go env  d (App f x) = go env d f $$ go env d x
  go env  d (Lam t)   = VLam $ \t' -> go (t':env) (d + 1) t
{-# inline evalList #-}  

nfList :: Term -> Term
nfList = quote . evalList

rawNf :: String -> Either String F.RawTerm
rawNf = fmap (toRaw . nfList . fromRaw) . F.parse

rnfTest :: String -> ()
rnfTest str = case F.parse str of
  Left  _ -> ()
  Right t -> rnf $ nfList (fromRaw t)



