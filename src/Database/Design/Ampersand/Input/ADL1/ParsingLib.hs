{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash, FlexibleInstances #-}
module Database.Design.Ampersand.Input.ADL1.ParsingLib(
--    UU.getMsgs,UU.parse,UU.evalSteps,UU.Pair(..),UU.Message(..),UU.Action(..),
    pSym, pSucceed, AmpParser,pAtom,
    (DF.<$>), (P.<|>), (<$), (CA.<*>), (CA.<*), (CA.*>), (<??>),
    pList, pList1, opt, pListSep, pList1Sep,pKey,pConid,pString,pSpec,pExpl,pVarid,pComma,pSemi,
    SourcePos, sourceName, sourceLine, sourceColumn
) where

import Control.Monad.Identity (Identity)
import Data.Char (isUpper)
import Database.Design.Ampersand.Input.ADL1.Lexer
import Database.Design.Ampersand.Input.ADL1.LexerToken (Token(..),Lexeme(..),lexemeLength)
import qualified Control.Applicative as CA
import qualified Data.Functor as DF
import qualified Text.Parsec.Prim as P
import Text.Parsec as P hiding(satisfy)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Token as P

--TODO: TokenMonad?
type AmpParser a = P.ParsecT [Token] SourcePos Identity a
type AmpLexer  a = P.ParsecT String  [Token]   Identity a

--Operators
infixl 4 <$
(<$) :: a -> AmpParser b -> AmpParser a
a <$ p = do { _ <- p; return a }

(<**>) :: AmpParser a -> AmpParser (a -> b) -> AmpParser b
p <**> q = (\ x f -> f x) CA.<$> p CA.<*> q

(<??>) :: AmpParser a -> AmpParser (a -> a) -> AmpParser a
p <??> q = p <**> (q `opt` id)

----------------------------------------------------------------------------------
-- Functions copied from Lexer after decision to split lexer and parser
----------------------------------------------------------------------------------

lexer :: GenTokenParser [Token] SourcePos Identity
lexer = makeTokenParser langDef

langDef :: GenLanguageDef [Token] SourcePos Identity
langDef = LanguageDef {
        commentStart = "{-",
        commentEnd = "-}",
        commentLine = "--",
        nestedComments = True,
        identStart = letter <|> char '_',
        identLetter = alphaNum <|> char '_',
        opStart = oneOf $ map head operators,
        opLetter = oneOf $ concat $ map tail operators,
        reservedNames = keywords,
        reservedOpNames = operators,
        caseSensitive = True
    }

pSym :: Token -> AmpParser Token
pSym = pSym

pSucceed :: a -> AmpParser a
pSucceed = P.parserReturn

pList :: AmpParser a -> AmpParser [a]
pList = P.many

pList1 ::  AmpParser a -> AmpParser [a]
pList1 = P.many1

pListSep :: AmpParser a -> AmpParser sep -> AmpParser [a]
pListSep = P.sepBy

pList1Sep ::  AmpParser a -> AmpParser sep -> AmpParser [a]
pList1Sep = P.sepBy1

opt ::  AmpParser a -> a -> AmpParser a
a `opt` b = P.option b a

-- Basic parsers
pKey :: String -> AmpParser ()
pKey key = reserved lexer $ key -- match (LexKeyword key)

--- Conid ::= UpperChar (Char | '_')*
pConid :: AmpParser String
pConid = P.lexeme lexer $ try $
        do name <- identifier lexer
           if isUpper $ head name
           then return name
           else unexpected ("Expected upper case identifier but got " ++ show name)

--- String ::= '"' Any* '"'
--- StringListSemi ::= String (';' String)*
pString :: AmpParser String
pString = stringLiteral lexer

-- Spec just matches the given character so it has no EBNF
pSpec :: Char -> AmpParser String
pSpec x = P.symbol lexer $ [x]

--- Expl ::= '{+' Any* '-}'
pExpl :: AmpParser String
pExpl = do _ <- try (string "{+")
           inExpl
        where inExpl =    do { _ <- try (str "+}");  return "explanation" }
                    P.<|> do { skipMany1 (str "+}"); inExpl } -- TODO: We shouldn't skip them of course
                    P.<?> "end of comment"
              str = reservedOp lexer

--- Varid ::= (LowerChar | '_') (Char | '_')*
pVarid :: AmpParser String
pVarid = P.lexeme lexer $ try $
        do name <- identifier lexer
           if isUpper $ head name
           then unexpected ("Expected lower case identifier but got " ++ show name)
           else return name

-- TODO: does not escape, i.e. 'Mario\'s Pizzas' will fail to parse
pAtom :: AmpParser String
pAtom   = P.lexeme lexer (
             do between (char '\'')
                        (char '\'' <?> "end of atom")
                        (many $ satisfy isLetter)
                <?> "atom")
            where isLetter c = (c /= '\'') && (c /= '\\') && (c > '\026')

--- Comma ::= ','
pComma :: AmpParser String
pComma  = pSpec ','

--- Semi ::= ';'
pSemi :: AmpParser String
pSemi = pSpec ';'

{- temp in comment as not specified in Lexer
instance Ord Tok where
    (<=) a b = show a <= show b
-}