
module Frontend (
    RawTerm(..)
  , Frontend.parse
  , scopeCheck
  ) where

import Control.Monad
import Control.Applicative
import Data.Bifunctor
import qualified Data.HashSet as S

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

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

-- | Fail on the first scope error, return the missing variable.
scopeCheck :: RawTerm -> Maybe String
scopeCheck = go S.empty where
  go s (Var v)   = v <$ guard (not $ S.member v s)
  go s (Lam v t) = go (S.insert v s) t
  go s (App f x) = go s f <|> go s x

parse :: String -> Either String RawTerm
parse str = do
  t <- first show $ Text.Megaparsec.parse term "" str
  maybe (pure t)
    (\v -> Left $ "Variable not in scope: " ++ v)
    (scopeCheck t)

 
