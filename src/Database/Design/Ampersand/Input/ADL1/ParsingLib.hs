{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash #-}
module Database.Design.Ampersand.Input.ADL1.ParsingLib ( UU.getMsgs,UU.parse,UU.evalSteps,UU.Pair(..),UU.Message(..),UU.Action(..),UUS.Pos(..), scan, initPos, UU.IsParser, pSym, pSucceed, UUS.Token(..), AmpParser, UUS.TokenType(..),UUS.noPos, (<$>), (<*>), (<|>), (<$), (<*), (*>), (<??>), pList, pList1, opt, pListSep, pList1Sep,pKey,pConid,pString,pSpec,pExpl,pVarid,pComma,pSemi ) where

import qualified UU.Parsing as UU
import qualified Database.Design.Ampersand.Input.ADL1.UU_Scanner as UUS

infixl 3 <|>
infixl 4 <*>, <$> 
infixl 4 <$, <*, *>

		 
type AmpParser a = UU.AnaParser [UUS.Token] UU.Pair UUS.Token (Maybe UUS.Token) a


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

pSym :: UU.IsParser p a => a -> p a
pSym = UU.pSym

pSucceed :: UU.IsParser p s => a -> p a
pSucceed = UU.pSucceed

pList :: UU.IsParser p s => p a -> p [a]
pList = UU.pList

pList1 :: UU.IsParser p s => p a -> p [a]
pList1 = UU.pList1

pListSep :: UU.IsParser p s => p a -> p a1 -> p [a1]
pListSep = UU.pListSep

pList1Sep :: UU.IsParser p s => p a -> p a1 -> p [a1]
pList1Sep = UU.pList1Sep


opt :: UU.IsParser p s => p a -> a -> p a
opt = UU.opt

-- Funtions from UU.Scanner

pKey :: UU.IsParser p UUS.Token => String -> p String
pKey = UUS.pKey

pConid :: UU.IsParser p UUS.Token => p String
pConid = UUS.pConid

pString :: UU.IsParser p UUS.Token => p String
pString = UUS.pString

pSpec :: UU.IsParser p UUS.Token => Char -> p String
pSpec = UUS.pSpec

pExpl :: UU.IsParser p UUS.Token => p String
pExpl = UUS.pExpl

pVarid :: UU.IsParser p UUS.Token => p String
pVarid = UUS.pVarid

pComma :: UU.IsParser p UUS.Token => p String
pComma = UUS.pComma

pSemi :: UU.IsParser p UUS.Token => p String
pSemi = UUS.pSemi

--Remark: description is different from the type check in GHCI!
scan :: [String] -> [String] -> String -> String -> String -> UUS.Pos -> String -> [UUS.Token]
scan = UUS.scan

--Remark: description is different from the type check in GHCI!
initPos :: UUS.Pos
initPos  = UUS.initPos