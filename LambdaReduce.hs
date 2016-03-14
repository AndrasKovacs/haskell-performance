
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import qualified Data.HashSet as S
import Control.Monad

data RawTerm = Var !String | Lam !String !RawTerm | App !RawTerm !RawTerm
  deriving Show

lexing :: (Parser () -> b) -> b
lexing =
  ($ (L.space (() <$ spaceChar) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")))

lexeme = lexing L.lexeme
keywords = S.fromList ["lam", "let", "in"]

symbol = try . lexing L.symbol

ident = try $ lexeme $ do
  id <- some (letterChar <|> char '_')
  guard $ not $ S.member id keywords
  pure id

letBind = do
  var <- symbol "let" *> ident <* symbol "="
  def <- term <* symbol "in"
  body <- term
  pure $ App (Lam var body) def

parens  = between (symbol "(") (symbol ")")
lamSym  = symbol "\\" <|> symbol "lam"
lam     = lamSym *> (flip (foldr Lam) <$> (some ident <* symbol ".") <*> term)
apps    = foldl1 App <$> some ((Var <$> ident) <|> parens term)
term    = lam <|> letBind <|> apps



-- module Main where

-- import Bound hiding (Var)
-- import Prelude.Extras
-- import Control.Monad

-- class ULC t where
--   type Var t
--   lam  :: Var t -> t -> t
--   var  :: Var t -> t
--   (@:) :: t -> t -> t
--   nf   :: t -> t
-- infixl 9 @:

-- -- z = lam "z" $ lam "s" $ var "z"
-- -- s n = lam "z" $ lam "s" $ var "s" @: (n @: var "z" @: var "s")
-- -- plus a b = lam "z" $ lam "s"

-- instance {-# OVERLAPS #-} (ULC t, Var t ~ String) => Num t where
--   a + b = lam "s" $ lam "z" $ a @: var "s" @: (b @: var "s" @: var "z")
--   a * b = lam "s" $ a @: (b @: var "s")
--   fromInteger n = lam "s" $ lam "z" $ go n where
--     go :: Integer -> t
--     go 0 = var "z"
--     go n = var "s" @: go (n - 1)



-- -- Bound
-- data BExp a = BV a | BApp (BExp a) (BExp a) | BLam (Scope () BExp a)
--   deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

-- instance Read1 BExp
-- instance Show1 BExp
-- instance Eq1 BExp
-- instance Ord1 BExp

-- instance Applicative BExp where
--   pure  = return
--   (<*>) = ap

-- instance Monad BExp where
--   return = BV
--   BV a     >>= f = f a
--   BApp a b >>= f = BApp (a >>= f) (b >>= f)
--   BLam e   >>= f = BLam (e >>>= f)

-- bnf :: BExp a -> BExp a
-- bnf (BLam t)   = BLam (toScope $ bnf $ fromScope t)
-- bnf (BApp f x) = case (bnf f, bnf x) of
--   (BLam t, x) -> bnf (instantiate1 x t)
--   (f     , x) -> BApp f x
-- bnf v = v

-- instance Eq a => ULC (BExp a) where
--   type Var (BExp a) = a
--   lam v b = BLam (abstract1 v b)
--   var     = BV
--   (@:)    = BApp
--   nf      = bnf





-- lam :: Eq a => (BExp a -> BExp a) -> BExp a
-- lam f = BLam (Scope _)


-- lam :: Eq a => a -> BExp a -> BExp a
-- lam v b = BLam (abstract1 v b)



-- foo = BApp (lam "a" $ lam "b" $ BApp (BV "a") (BV "a")) (BV "b")

