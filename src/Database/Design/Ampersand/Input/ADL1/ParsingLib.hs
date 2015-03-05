{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash, FlexibleInstances #-}
module Database.Design.Ampersand.Input.ADL1.ParsingLib(
    UU.getMsgs,UU.parse,UU.evalSteps,UU.Pair(..),UU.Message(..),UU.Action(..),
    pSym, pSucceed, AmpParser,pAtom,
    (<$>), (<*>), (<|>), (<$), (<*), (*>), (<??>),
    pList, pList1, opt, pListSep, pList1Sep,pKey,pConid,pString,pSpec,pExpl,pVarid,pComma,pSemi,
    SourcePos, sourceName, sourceLine, sourceColumn
) where

import qualified UU.Parsing as UU
import Text.Parsec.Pos (SourcePos, sourceName, sourceLine, sourceColumn)
import Database.Design.Ampersand.Input.ADL1.LexerToken (Token(..), TokenType(..))
import Database.Design.Ampersand.Input.ADL1.Lexer
import Control.Monad.Identity (Identity)
import Data.Char (isUpper)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim as P
import Text.Parsec.Token
import qualified Data.Functor as DF
import qualified Control.Applicative as CA

infixl 4 <*>, <$> 
infixl 4 <$, <*, *>

--TODO: TokenMonad?
type AmpParser a = ParsecT [Token] SourcePos Identity a
type AmpT a      = ParsecT String  [Token] Identity a

--Operators from UU.Parsing
(<$>) :: (a -> b) -> AmpParser a -> AmpParser b
(<$>) =  (DF.<$>)

(<|>) :: AmpParser a -> AmpParser a -> AmpParser a
(<|>) =  (P.<|>)

(<*>) :: AmpParser (a -> b) -> AmpParser a -> AmpParser b
(<*>)  = (CA.<*>)

(<*) :: AmpParser a -> AmpParser b -> AmpParser a
(<*) = (CA.<*)

(<$) :: AmpParser (a -> b) -> AmpParser a -> AmpParser b
(<$) = (CA.<$)

(*>) :: AmpParser a -> AmpParser b -> AmpParser b
(*>) =  (CA.*>)

(<**>) :: AmpParser a -> AmpParser (a -> b) -> AmpParser b
p <**> q = (\ x f -> f x) CA.<$> p <*> q

(<??>) :: AmpParser a -> AmpParser (a -> a) -> AmpParser a
p <??> q = p <**> (q `opt` id)

--Functions from UU.Parsing
----------------------------------------------------------------------------------
-- Functions copied from Lexer after decision to split lexer and parser
----------------------------------------------------------------------------------

lexer :: TokenParser [Token]
lexer = makeTokenParser langDef

langDef :: LanguageDef [Token]
langDef = LanguageDef {
        commentStart = "{-",
        commentEnd = "-}",
        commentLine = "--",
        nestedComments = True,
        identStart = letter P.<|> char '_',
        identLetter = alphaNum P.<|> char '_',
        opStart = oneOf $ map head operators,
        opLetter = oneOf $ concat $ map tail operators,
        reservedNames = keywords,
        reservedOpNames = operators,
        caseSensitive = True
    }

pSym :: Token -> AmpParser Token
pSym = pSym

pSucceed :: a -> AmpParser a
pSucceed = UU.pSucceed

pList :: AmpParser a -> AmpParser [a]
pList = UU.pList

pList1 ::  AmpParser a -> AmpParser [a]
pList1 = UU.pList1

pListSep :: AmpParser a -> AmpParser a1 -> AmpParser [a1]
pListSep = UU.pListSep

pList1Sep ::  AmpParser a -> AmpParser a1 -> AmpParser [a1]
pList1Sep = UU.pList1Sep

opt ::  AmpParser a -> a -> AmpParser a
opt = UU.opt

instance UU.Symbol Token where
    deleteCost t = 0#
    symBefore t = t
    symAfter t = t

	
pKey :: String -> AmpT ()
pKey = reserved lexer

--- Conid ::= UpperChar (Char | '_')*
pConid :: AmpT String
pConid = lexeme lexer $ try $
        do name <- identifier lexer
           if isUpper $ head name
           then return name
           else unexpected ("Expected upper case identifier but got " ++ show name)

--- String ::= '"' Any* '"'
--- StringListSemi ::= String (';' String)*
pString :: AmpT String
pString = stringLiteral lexer

-- Spec just matches the given character so it has no EBNF
pSpec :: Char -> AmpT String
pSpec x = do { y <- char x; return [y] }

--- Expl ::= '{+' Any* '-}'
pExpl :: AmpT String
pExpl = do try (string "{+")
           inExpl
        where inExpl =  do{ try (string "+}")            ; return "explanation" }
                    P.<|> do{ skipMany1 (noneOf "+}")      ; inExpl } -- TODO: We shouldn't skip them of course
                    P.<?> "end of comment"

--- Varid ::= (LowerChar | '_') (Char | '_')*
pVarid :: AmpT String
pVarid = lexeme lexer $ try $
        do name <- identifier lexer
           if isUpper $ head name
           then unexpected ("Expected lower case identifier but got " ++ show name)
           else return name

-- TODO: does not escape, i.e. 'Mario\'s Pizzas' will fail to parse
pAtom :: AmpT String
pAtom   = lexeme lexer (
             do between (char '\'')
                        (char '\'' <?> "end of atom")
                        (many $ satisfy isLetter)
                <?> "atom")
            where isLetter c = (c /= '\'') && (c /= '\\') && (c > '\026')
	
--- Comma ::= ','
pComma :: AmpT String
pComma  = pSpec ','

--- Semi ::= ';'
pSemi :: AmpT String
pSemi = pSpec ';'

{- temp in comment as not specified in Lexer
instance Ord Tok where
    (<=) a b = show a <= show b
-}