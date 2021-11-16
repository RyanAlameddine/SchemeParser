# ParserCombinators

Simple Monadic parser combinator library written in Haskell and a Json parser written using it.

Parser type definition:
```hs
newtype Parser a = Parser (String -> Maybe (String, a))

runParser :: Parser a -> String -> Maybe (String, a)
runParser (Parser f) = f
```

Typeclass instances

```hs
instance Functor Parser where
    fmap f (Parser p) = Parser parser
        where
            parser xs = do
                (str, val) <- p xs
                Just (str, f val)

instance Applicative Parser where
    pure v = Parser (\xs = Just (xs, v))
    liftA2 f x y = Parser parser
        where
            parser input = do
                (input' , v1) <- runParser x input
                (input'', v2) <- runParser y input'
                Just (input'', f v1 v2)

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser a) <|> (Parser b) = Parser parser
        where 
            parser input = a input <|> b input
```

JSON representation:
```hs
data JsonValue = JsonNull
                | JsonBool Bool
                | JsonNumber Integer 
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject [(String, JsonValue)]
                deriving Show
```

Example JSON parsers:
```hs
jsonBool = (JsonBool True <$ string "true") <|> (JsonBool False <$ string "false")

jsonNumber = JsonNumber <$> int

jsonString = JsonString <$> stringLiteral

jsonArray = JsonArray <$> (char '[' *> values <* char ']')
    where 
        values = separateBy comma jsonValue

jsonObject = JsonObject <$> (char '{' *> object <* char '}')
    where
        object = separateBy comma pair
        pair = combine (name <* char ':') jsonValue
        combine = liftA2 (,)
        name = zeroOrMoreSpace *> stringLiteral <* zeroOrMoreSpace 

stringLiteral = doubleQuote *> many stringChar <* doubleQuote
stringChar = satisfy (/='"')
```