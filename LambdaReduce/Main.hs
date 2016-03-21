
module Main where

import Control.DeepSeq

import qualified Frontend as F
import NBE

{- Notes

Todo:
  - hsubst without Bloom
  - NBE via GHCI/GHC
  - Naive subst
  - de Bruijn
  - Bound

  - More bench, more complex programs preferably with trees & lots of bindings
  - Bench for hsubst specifically (subst normal term into large normal term)

  - Can we adapt NBE for efficient hsubst?
-}

main = do
  -- print $ rnfTest test
  print $ rnf (quote res)

z       = VLam $ \s -> VLam $ \z -> z
s       = VLam $ \n -> VLam $ \s -> VLam $ \z -> s $$ (n $$ s $$ z)
plus    = VLam $ \a -> VLam $ \b -> VLam $ \s -> VLam $ \z -> a $$ s $$ (b $$ s $$ z)
mult    = VLam $ \a -> VLam $ \b -> VLam $ \s -> VLam $ \z -> a $$ (b $$ s) $$ z
ten     = mult $$ (s $$ (s $$ z)) $$ (s $$ (s $$ (s $$ (s $$ (s $$ z)))))
hundred = mult $$ ten $$ ten
res     = mult $$ ten $$ (mult $$ hundred $$ (mult $$ hundred $$ hundred))
{-# inline z       #-}
{-# inline s       #-}
{-# inline plus    #-}
{-# inline mult    #-}
{-# inline ten     #-}
{-# inline hundred #-}
{-# inline res     #-}

test = unlines [
  "let z = lam s z.z in",
  "let s = lam n s z. s (n s z) in",
  "let plus = lam a b s z. a s (b s z) in",
  "let mult = lam a b s z. a (b s) z in",
  "let 10 = mult (s (s z)) (s (s (s (s (s z))))) in",
  "let 100 = mult 10 10 in",
  "mult 10 (mult 100 (mult 100 100))"
  ]


