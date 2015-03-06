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
import Database.Design.Ampersand.Input.ADL1.LexerToken
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

--TODO: The patters here are not always necessary
--TODO: This function is hard to understand.
checkTok :: (Token -> Maybe a) -> AmpParser a
checkTok pred
  = tokenPrimEx
        showtok 
        nextpos 
        (Just (\oldpos (Tok lex pos) lexemes old -> incSourceColumn pos (lexemeLength lex)))
        pred
  where  showtok (Tok lex pos)   = show lex
         nextpos _ _ ((Tok lex pos):_) = pos
         nextpos pos _ [] = pos

check :: (Lexeme -> Maybe a) -> AmpParser a
check pred = checkTok (\(Tok l _) -> pred l)

match :: Lexeme -> AmpParser String
match lex = check (\lex' -> if (lex == lex') then Just (get_lex_val lex) else Nothing) <?> show lex

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
-- TODO: Maybe we wanna make functions here for the different keywords.
pKey :: String -> AmpParser String
pKey key = match (LexKeyword key)

--- Conid ::= UpperChar (Char | '_')*
pConid :: AmpParser String
pConid = check (\lex -> case lex of { LexUpperId s -> Just s; other -> Nothing })

--- String ::= '"' Any* '"'
--- StringListSemi ::= String (';' String)*
pString :: AmpParser String
pString = check (\lex -> case lex of { LexString s -> Just s; other -> Nothing })

-- Spec just matches the given character so it has no EBNF
--TODO: This should not be available for the parser, we can make the abstraction in this lib.
pSpec :: Char -> AmpParser String
pSpec sym = match (LexSymbol [sym])

--- Expl ::= '{+' Any* '-}'
pExpl :: AmpParser String
pExpl = check (\lex -> case lex of { LexExpl s -> Just s; other -> Nothing })

--- Varid ::= (LowerChar | '_') (Char | '_')*
pVarid :: AmpParser String
pVarid = check (\lex -> case lex of { LexLowerId s -> Just s; other -> Nothing })

-- TODO: does not escape, i.e. 'Mario\'s Pizzas' will fail to parse
pAtom :: AmpParser String
pAtom = check (\lex -> case lex of { LexAtom s -> Just s; other -> Nothing })

--TODO: No basic parsers for the following lexemes
-- LexOp          String
-- LexChar        Char
-- LexInteger     Int
-- LexTextName    String
-- LexTextLine    String
-- LexSpace

--- Comma ::= ','
pComma :: AmpParser String
pComma  = pSpec ','

--- Semi ::= ';'
pSemi :: AmpParser String
pSemi = pSpec ';'

posOf :: AmpParser a -> AmpParser Origin
posOf parse = do { t <- parse; return (get_tok_pos t) }

valPosOf :: AmpParser a -> AmpParser (a, Origin)
valPosOf parse = do { t <- parse; return (get_tok_val t,get_tok_pos t) }

pKey_pos :: String -> AmpParser Origin
pKey_pos = posOf.pKey

pSpec_pos :: Char -> AmpParser Origin
pSpec_pos = posOf.pSpec

pString_val_pos, pVarid_val_pos, pConid_val_pos, pAtom_val_pos ::  AmpParser (String,Origin)
pString_val_pos = posOf.pString
pVarid_val_pos  = posOf.pVarid
pConid_val_pos  = posOf.pConid
pAtom_val_pos   = posOf.pAtom

pKey_val_pos ::  String -> AmpParser (String,Origin)
pKey_val_pos keyword = posOf.pKey
