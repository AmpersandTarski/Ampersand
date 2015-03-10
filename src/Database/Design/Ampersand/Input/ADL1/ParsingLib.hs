{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash, FlexibleInstances #-}
module Database.Design.Ampersand.Input.ADL1.ParsingLib(
--    UU.getMsgs,UU.parse,UU.evalSteps,UU.Pair(..),UU.Message(..),UU.Action(..),
    pSym, pSucceed, AmpParser,pAtom,
    (DF.<$>), (P.<|>), (<$), (CA.<*>), (CA.<*), (CA.*>), (<??>),
    pList, pList1, opt, pListSep, pList1Sep, try,
    pKey,pConid,pString,pSpec,pExpl,pVarid,pComma,pSemi,
    pString_val_pos, pVarid_val_pos, pConid_val_pos, pAtom_val_pos,
    pKey_val_pos, pKey_pos, pSpec_pos,
    SourcePos, sourceName, sourceLine, sourceColumn, posOrigin
) where

import Control.Monad.Identity (Identity)
import Database.Design.Ampersand.Input.ADL1.LexerToken
import qualified Control.Applicative as CA
import qualified Data.Functor as DF
import qualified Text.Parsec.Prim as P
import Text.Parsec as P hiding(satisfy)

type AmpParser a = P.ParsecT [Token] SourcePos Identity a

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

--TODO: The patterns here are not always necessary
--TODO: This function is hard to understand.
check :: (Lexeme -> Maybe a) -> AmpParser a
check predicate
  = tokenPrimEx
        showTok -- Token pretty-printing function.
        nextPos -- Next position calculating function.
        (Just nextState) -- The next user state
        (\(Tok l _) -> predicate l) -- ^ Matching function for the token to parse.
  where  showTok :: Token -> String
         showTok (Tok lx _)   = show lx
         nextPos :: SourcePos -> Token -> [Token] -> SourcePos
         nextPos pos _ [] = pos
         nextPos _ _ ((Tok _ pos):_) = pos
         nextState :: SourcePos -> Token -> [Token] -> SourcePos -> SourcePos
         nextState _ (Tok lx pos) _ _ = incSourceColumn pos (lexemeLength lx)

match :: Lexeme -> AmpParser String
match lx = check (\lx' -> if (lx == lx') then Just (get_lex_val lx) else Nothing) <?> show lx

pSym :: Token -> AmpParser Token
pSym = pSym

pSucceed :: a -> AmpParser a
pSucceed = P.parserReturn

pList :: AmpParser a -> AmpParser [a]
pList = P.many

pList1 ::  AmpParser a -> AmpParser [a]
pList1 = P.many1

pListSep :: AmpParser sep -> AmpParser a -> AmpParser [a]
pListSep sep a = P.sepBy a sep

pList1Sep ::  AmpParser sep -> AmpParser a -> AmpParser [a]
pList1Sep sep a = P.sepBy1 a sep

opt ::  AmpParser a -> a -> AmpParser a
a `opt` b = P.option b a

-- Basic parsers
-- TODO: Maybe we wanna make functions here for the different keywords.
pKey :: String -> AmpParser String
pKey key = match (LexKeyword key)

--- Conid ::= UpperChar (Char | '_')*
pConid :: AmpParser String
pConid = check (\lx -> case lx of { LexUpperId s -> Just s; _ -> Nothing })

--- String ::= '"' Any* '"'
--- StringListSemi ::= String (';' String)*
pString :: AmpParser String
pString = check (\lx -> case lx of { LexString s -> Just s; _ -> Nothing })

--TODO: This should not be available for the parser, we can make the abstraction in this lib.
-- matches special characters
pSpec :: Char -> AmpParser String
pSpec sym = match (LexSymbol [sym])

--- Expl ::= '{+' Any* '-}'
pExpl :: AmpParser String
pExpl = check (\lx -> case lx of { LexExpl s -> Just s; _ -> Nothing })

--- Varid ::= (LowerChar | '_') (Char | '_')*
pVarid :: AmpParser String
pVarid = check (\lx -> case lx of { LexLowerId s -> Just s; _ -> Nothing })

-- TODO: does not escape, i.e. 'Mario\'s Pizzas' will fail to parse
pAtom :: AmpParser String
pAtom = check (\lx -> case lx of { LexAtom s -> Just s; _ -> Nothing })

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

posOrigin :: Show a => a -> SourcePos -> Origin
posOrigin sym p = FileLoc (FilePos (sourceName p, p, show sym))

posOf :: Show a => AmpParser a -> AmpParser Origin
posOf parser = do { pos <- getPosition; a <- parser; return (posOrigin a pos) }

valPosOf :: Show a => AmpParser a -> AmpParser (String, Origin)
valPosOf parser = do { pos <- getPosition; a <- parser; return (show a, posOrigin a pos) }

pString_val_pos, pVarid_val_pos, pConid_val_pos, pAtom_val_pos ::  AmpParser (String,Origin)
pString_val_pos = valPosOf pString
pVarid_val_pos  = valPosOf pVarid
pConid_val_pos  = valPosOf pConid
pAtom_val_pos   = valPosOf pAtom

pKey_val_pos ::  String -> AmpParser (String,Origin)
pKey_val_pos = valPosOf.pKey

pKey_pos :: String -> AmpParser Origin
pKey_pos = posOf.pKey

pSpec_pos :: Char -> AmpParser Origin
pSpec_pos = posOf.pSpec
