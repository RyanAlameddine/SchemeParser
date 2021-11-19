# Scheme Parser

A scheme parser I wrote (inspired by the Berkeley CS61A interpreter project) using my Monadic/Applicative parser combinator library. The entire codebase is written in Haskell. Scroll down to the bottom to see an example use case!

# Usage

If you would like to test out this project, feel free to download it and experiment or submit pull requests! The main program can be executed by calling `stack build` and `stack run` (with haskell stack installed). If you would like to experiment with the code, I reccomend entereing the `stack ghci` and the loading SchemeParser.hs.

# The Parser Combinator Library

Parsering functions are defined as combinations of other simple parsers, built from the ground up. Here is the implementation for the parser type as well as some simple base parsers:


Parser type definition:
```hs
#record type autogenerates runParser function that extracts the inner value (the parser function)
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }
```

Typeclass instances:

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

instance Monad Parser where
    return = pure
    (Parser x) >>= f = Parser parser
        where
            parser input = do
                (input' , v1) <- x input
                runParser (f v1) input'
```

# The Scheme Parser

The scheme parser itself, built on top of the parser combinator library, is a combination of two things:
1) The Scheme objects - a haskell representation of all the parse items in the structure of a scheme program represented using discriminated unions. 
2) The Scheme parsers - these are Parser objects which represent functions which parse parts of a string into a corresponding scheme object

*Note: this was entirely taken from the Scheme language definition found (here)[https://www.scheme.com/tspl2d/grammar.html]*

Scheme Objects:
```hs
type Program = [Form]

data Form =
    FDefinition Definition
    | FExpression Expression
    deriving Show

data Definition =
    DVariable [Variable] Body
    deriving Show

data Expression =
    EConstant Constant
    | EVariable Variable
    | ELambda [Variable] Body
    | EIf Expression Expression (Maybe Expression)
    | EApplication Expression [Expression]
    deriving Show

data Constant =
    CBool Bool
    | CNumber Integer
    | CCharacter Char
    | CString String
    deriving Show

data Body =
    Body [Definition] [Expression]
    deriving Show

type Variable = String
```

The parsers themselves were designed to mimic the language grammar itself for maximum clarity.

Example Scheme Parsers:
```hs
#note, for example, that an expression is simply a choice between a quote, lambda, if, application, constant, or variable
expression :: Parser Expression
expression = choice [eQuote, eLambda, eIf, eApp, eConstant, eVariable]

#a quote expression is a ' followed by an expression, but should be parsed as (EApplication (Evariable "quote") expression)
eQuote :: Parser Expression
eQuote = char '\'' *> (EApplication (EVariable "quote") <$> (singleton <$> expression))

#a constant is a choice between a boolean, a number, a char, or a string
eConstant :: Parser Expression
eConstant = EConstant <$> choice [cBool, cNumber, cChar, cStr]
    where
        cNumber = CNumber <$> int
        cChar   = CCharacter <$> (char '#' *> anyChar)
        cBool   = CBool True <$ string "#t" <|> CBool False <$ string "#f"
        cStr    = CString <$> surroundWith '"' (many pStringChar)

eIf :: Parser Expression
eIf = pList "if " $ EIf <$> expression <*> expression <*> optional expression

eApp :: Parser Expression
eApp = pList "" $ EApplication <$> expression <*> many (someSpace *> expression)

eLambda :: Parser Expression
eLambda = pList "lambda " $ ELambda <$> pFormals <*> (someSpace *> pBody)

pFormals :: Parser [Variable]
pFormals = (:[]) <$> pVariable <|> pManyList "" pVariable

pBody :: Parser Body
pBody = Body <$> separateBy someSpace definition <*> someSeparateBy someSpace expression
```

## Example execution

```scheme
Enter the scheme program code you would like to parse:
(define x (lambda y (if (= y #t) (+ 2 y) "hello"))) 

[FDefinition (DVariable ["x"] (Body [] [ELambda ["y"] (Body [] [EApplication (EVariable "if") [EApplication (EVariable "=") [EVariable "y",EConstant (CBool True)],EApplication (EVariable "+") [EConstant (CNumber 2),EVariable "y"],EConstant (CString "hello")]])]))]
```

This program's use is fairly simple. It inputs a string which represents a scheme program. It outputs an sort of tree structure which represents that program. Notice that the top level is a Program (List of Forms). Our one Form is an Definition. That definition has a list of variables, and then a body. That body contains a Lambda which contains another set of variables and a body etc. As you can see, it is arranged into a tree structure that represents the structure and type of each syntactic construct.

Notice also the parsing of constants, for example #t, are represnted in the haskell value as their equivalent values like `True`, or `2`, or `"hello"`.