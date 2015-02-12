{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash #-}
module Database.Design.Ampersand.Input.ADL1.ParsingLib ( UU.getMsgs,UU.parse,UU.evalSteps,UU.Pair(..),UU.Message(..),UU.Action(..),UU.Pos(..), scan, initPos, pSym, pSucceed, UU.Token(..), AmpParser, UU.TokenType(..),UU.noPos, (<$>), (<*>), (<|>), (<$), (<*), (*>), (<??>), pList, pList1, opt, pListSep, pList1Sep,pKey,pConid,pString,pSpec,pExpl,pVarid,pComma,pSemi ) where

import qualified UU.Parsing as UU
import qualified Database.Design.Ampersand.Input.ADL1.UU_Scanner as UU

infixl 3 <|>
infixl 4 <*>, <$> 
infixl 4 <$, <*, *>

		 
type AmpParser a = UU.AnaParser [UU.Token] UU.Pair UU.Token (Maybe UU.Token) a

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

pSym :: UU.Token -> AmpParser UU.Token
pSym = UU.pSym

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

-- Funtions from UU.Scanner

pKey :: UU.IsParser p UU.Token => String -> p String
pKey = UU.pKey

pConid :: UU.IsParser p UU.Token => p String
pConid = UU.pConid

pString :: UU.IsParser p UU.Token => p String
pString = UU.pString

pSpec :: UU.IsParser p UU.Token => Char -> p String
pSpec = UU.pSpec

pExpl :: UU.IsParser p UU.Token => p String
pExpl = UU.pExpl

pVarid :: UU.IsParser p UU.Token => p String
pVarid = UU.pVarid

pComma :: UU.IsParser p UU.Token => p String
pComma = UU.pComma

pSemi :: UU.IsParser p UU.Token => p String
pSemi = UU.pSemi

--Remark: description is different from the type check in GHCI!
scan :: [String] -> [String] -> String -> String -> String -> UU.Pos -> String -> [UU.Token]
scan = UU.scan

--Remark: description is different from the type check in GHCI!
initPos :: UU.Pos
initPos  = UU.initPos