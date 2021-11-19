module SchemeParser where

import ParserLib
import Control.Applicative

-- parseFile :: FilePath -> IO (Maybe (String, SchemeObject))
-- parseFile path = do
--     text <- readFile path 
--     return $ runParser schemeFile text 

-- items taken from grammar https://www.scheme.com/tspl2d/grammar.html

--this is simplified
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

program :: Parser Program
program = manySpace *> many (form <* manySpace)

form :: Parser Form
form = (FDefinition <$> definition) <|> (FExpression <$> expression)

definition :: Parser Definition
definition = choice [dVariable]

dVariable :: Parser Definition
dVariable = pList "define " $ (single <|> list) <*> (someSpace *> pBody)
    where
        single = DVariable <$> (singleton <$> pVariable)
        list = DVariable <$> pSomeList "" pVariable

expression :: Parser Expression
expression = choice [eQuote, eLambda, eIf, eApp, eConstant, eVariable]

eQuote :: Parser Expression
eQuote = char '\'' *> (EApplication (EVariable "quote") <$> (singleton <$> expression))

eConstant :: Parser Expression
eConstant = EConstant <$> choice [cBool, cNumber, cChar, cStr]
    where
        cNumber = CNumber <$> int
        cChar   = CCharacter <$> (char '#' *> anyChar)
        cBool   = CBool True <$ string "#t" <|> CBool False <$ string "#f"
        cStr    = CString <$> surroundWith '"' (many pStringChar)

eVariable :: Parser Expression
eVariable = EVariable <$> pVariable

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

pList str p = surroundWith' '(' ')' $ manySpace *> string str *> manySpace *> p <* manySpace

pManyList s = pList s . separateBy someSpace
pSomeList s = pList s . someSeparateBy someSpace

pVariable = ((:) <$> initial <*> many subsequent) <|> ((:[]) <$> oneOfChar "+-")
    where
        initial = alpha <|> oneOfChar "!$%&*/:<=>?~_^"
        subsequent = alphaNum <|> oneOfChar ".+-"

pStringChar = satisfy (/='"')

singleton a = [a]