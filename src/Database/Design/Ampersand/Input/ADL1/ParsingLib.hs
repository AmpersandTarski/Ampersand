{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash #-}
module Database.Design.Ampersand.Input.ADL1.ParsingLib ( UU.getMsgs,UU.parse,UU.evalSteps,UU.Pair(..),UU.Message(..),UU.Action(..),UU.Pos(..), scan, initPos, pSym, pSucceed, AmpParser, UU.noPos, (<$>), (<*>), (<|>), (<$), (<*), (*>), (<??>), pList, pList1, opt, pListSep, pList1Sep,pKey,pConid,pString,pSpec,pExpl,pVarid,pComma,pSemi ) where

import qualified UU.Parsing as UU
import Database.Design.Ampersand.Input.ADL1.LexerToken (Token(..), SourcePos)
import Database.Design.Ampersand.Input.ADL1.Lexer

infixl 3 <|>
infixl 4 <*>, <$> 
infixl 4 <$, <*, *>

type AmpParser a = UU.AnaParser [Token] UU.Pair Token (Maybe Token) a

--Operators from UU.Parsing
(<$>) :: UU.IsParser p s => (a->b)   -> p a -> p b
(<$>) =  (UU.<$>)

(<|>) :: UU.IsParser p s => p a -> p a -> p a
(<|>) =  (UU.<|>)

(<*>) :: UU.IsParser p s => p (a -> b) -> p a -> p b
f <*> g  = f UU.<*> g


(<*) :: UU.IsParser p s => p a -> p b -> p a
(<*) =  (UU.<*)

(<$) :: UU.IsParser p s => b -> p a -> p b
(<$) =  (UU.<$)

(*>) :: UU.IsParser p s => p a -> p b -> p b
(*>) =  (UU.*>)

(<??>) :: UU.IsParser p s => p a -> p (a -> a) -> p a
(<??>) = (UU.<??>) 

--Functions from UU.Parsing

pSym :: Token -> AmpParser Token
pSym = pSym

pSucceed ::  a -> AmpParser a
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
