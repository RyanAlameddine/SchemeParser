module ParserLib where

import Control.Applicative ( Applicative(liftA2), Alternative(..) )
import Data.Char ( isDigit, isSpace, isAlpha, isAlphaNum )

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser parse
    where
        parse (x:xs)
            | p x = Just (xs, x)
        parse _ = Nothing

char :: Char -> Parser Char
char c = satisfy (==c)

digit :: Parser Char
digit = satisfy isDigit

alpha :: Parser Char 
alpha = satisfy isAlpha

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

instance Functor Parser where
    fmap f (Parser p) = Parser parser
        where
            parser xs = do
                val <- p xs
                Just $ (f <$> val)

instance Applicative Parser where
    pure v = Parser parser
        where parser xs = Just (xs, v)
    liftA2 f (Parser x) (Parser y) = Parser parser
        where
            parser input = do
                (input' , v1) <- x input
                (input'', v2) <- y input'
                pure (input'', f v1 v2)

string :: String -> Parser String
string = sequenceA . map char

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser a) <|> (Parser b) = Parser (\input -> a input <|> b input)

int :: Parser Integer
int = read <$> some digit

choice :: [Parser a] -> Parser a
choice = foldl1 (<|>)

space :: Parser Char
space = satisfy isSpace

manySpace :: Parser [Char]
manySpace = many space
someSpace :: Parser [Char]
someSpace = some space

separateBy :: Parser a -> Parser b -> Parser [b]
separateBy sep val = someSeparateBy sep val <|> pure []

someSeparateBy :: Parser a -> Parser b -> Parser [b]
someSeparateBy sep val = (:) <$> val <*> many (sep *> val)

anyChar :: Parser Char
anyChar = satisfy $ const True

oneOfChar :: [Char] -> Parser Char 
oneOfChar cs = satisfy (`elem` cs)

surroundWith :: Char -> Parser a -> Parser a
surroundWith c p = char c *> p <* char c
surroundWith' :: Char -> Char -> Parser a -> Parser a
surroundWith' c1 c2 p = char c1 *> p <* char c2

--runParser (separateBy (char ',') (int)) "123,1234,12,5"