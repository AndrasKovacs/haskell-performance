
module Frontend (
    RawTerm(..)
  , Frontend.parse
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

parse :: String -> Either String RawTerm
parse = first show . Text.Megaparsec.parse term ""

 
