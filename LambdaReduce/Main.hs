
module Main where

import qualified Frontend as F
import qualified Bloom
import qualified NBE

main = print $ NBE.rawNf test

test = unlines [
  "let z = lam s z.z in",
  "let s = lam n s z. s (n s z) in",
  "let plus = lam a b s z. a s (b s z) in",
  "let mult = lam a b s z. a (b s) z in",
  "let 10 = mult (s (s z)) (s (s (s (s (s z))))) in",
  "let 100 = mult 10 10 in",
  "mult 10 10"
  ]


