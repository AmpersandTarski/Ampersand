{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Ampersand.FSpec.ToFSpec.Calc
            ( deriveProofs
            , showProof, showPrf, conjuncts
            , commaEngPandoc, commaNLPandoc, commaEngPandoc', commaNLPandoc', commaPandocAnd ,commaPandocOr--TODO: this shouldt be here!
            , quadsOfRules
            ) where

import Ampersand.Basics
import Data.List hiding (head)
import Data.Monoid
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Classes
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.ShowADL (ShowADL(..))
import Ampersand.FSpec.ToFSpec.NormalForms
import Ampersand.Misc (Lang(..),Options(..))
import Text.Pandoc.Builder

testConfluence :: A_Context -> Blocks
testConfluence context
 = let tcss = [(expr,tcs) | expr<-expressionsIn context, let tcs=dfProofs expr, length tcs>1]
       sumt = sum (map (length.snd) tcss)
   in
   para ("Confluence analysis statistics from "<>(str.show.length.expressionsIn) context<>" expressions."<>linebreak)<>
   para ("This script contains "<>linebreak<>(str.show.length) tcss<> " non-confluent expressions "<>linebreak)<>
   para (linebreak<>"Total number of derived expressions: "<>(str.show) sumt<>linebreak)<>
   para ("Confluence analysis for "<>(str.name) context)<>
   mconcat
     [ para (linebreak<>"expression:   "<>(str . showADL) expr<>linebreak)<>
       bulletList [ showProof (para.str.showADL) prf | (_,prf)<-tcs ]
     | (expr,tcs)<-tcss]

deriveProofs :: Options -> A_Context -> Blocks
deriveProofs opts context
 = testConfluence context<>
   para (linebreak<>"--------------"<>linebreak)<>
   para ("Rules and their conjuncts for "<>(str.name) context)<>
   bulletList [ para ("rule r:   "<>str (name r)<>linebreak<>
                      "rrexp r:  "<>str (showADL (rrexp r))<>linebreak<>
                      "conjNF:   "<>str (showADL (conjNF opts (rrexp r)))<>linebreak<>
                      interText linebreak [ "     conj: "<>str (showADL conj) | conj<-conjuncts opts r ]
                     )
              | r<-allRules context]
   
   where
--    interText :: (Data.String.IsString a, Data.Monoid.Monoid a) => a -> [a] -> a
    interText _ [] = ""
    interText inbetween (xs:xss) = xs<>inbetween<>interText inbetween xss


type Proof expr = [(expr,[String],String)]

showProof :: (expr->Blocks) -> Proof expr -> Blocks
showProof shw [(expr,ss,_)]       = shw expr<> para ( str(" { "++intercalate " and " ss++" }"))
showProof shw ((expr,ss,equ):prf) = shw expr<>
                                    para (if null ss  then str equ else
                                          if null equ then str (unwords ss) else
                                          str equ<>str (" { "++intercalate " and " ss++" }"))<>
                                    showProof shw prf
                                    --where e'= if null prf then "" else let (expr,_,_):_ = prf in showHS options "" expr
showProof _  []                   = fromList []

-- showPrf is meant to circumvent Pandoc. For example when a proof needs to be shown in debugging texts.
showPrf :: (expr->String) -> Proof expr -> [String]
showPrf shw [(expr,_ ,_)]       = [ "    "++shw expr]
showPrf shw ((expr,ss,equ):prf) = [ "    "++shw expr] ++
                                  (if null ss  then [ equ ] else
                                   if null equ then [ unwords ss ] else
                                   [ equ++" { "++intercalate " and " ss++" }" ])++
                                  showPrf shw prf
showPrf _  []                   = []



commaEngPandoc' :: Inlines -> [Inlines] -> Inlines
commaEngPandoc' s [a,b,c] = a <> ", " <> b <> ", " <> s <> space <> c
commaEngPandoc' s [a,b]   = a <> space <> s <> space <> b
commaEngPandoc' _   [a]   = a
commaEngPandoc' s (a:as)  = a <> ", " <> commaEngPandoc' s as
commaEngPandoc' _   []    = mempty

commaEngPandoc :: Inline -> [Inline] -> [Inline]
commaEngPandoc s [a,b,c] = [a,Str ", ",b,Str ", ",s, Str " ", c]
commaEngPandoc s [a,b]   = [a,Str " ",s, Str " ", b]
commaEngPandoc _   [a]   = [a]
commaEngPandoc s (a:as)  = [a, Str ", "]++commaEngPandoc s as
commaEngPandoc _   []    = []

commaNLPandoc' :: Inlines -> [Inlines] -> Inlines
commaNLPandoc' s [a,b]  = a <> space <> s <> space <> b
commaNLPandoc'  _  [a]  = a
commaNLPandoc' s (a:as) = a <> ", " <> commaNLPandoc' s as
commaNLPandoc'  _  []   = mempty
commaNLPandoc :: Inline -> [Inline] -> [Inline]
commaNLPandoc s [a,b]  = [a,Str " ",s, Str " ", b]
commaNLPandoc  _  [a]  = [a]
commaNLPandoc s (a:as) = [a, Str ", "]++commaNLPandoc s as
commaNLPandoc  _  []   = []
   
commaPandocAnd :: Lang -> [Inlines] -> Inlines
commaPandocAnd Dutch = commaNLPandoc' "en"
commaPandocAnd English = commaEngPandoc' "and"
commaPandocOr :: Lang -> [Inlines] -> Inlines
commaPandocOr Dutch = commaNLPandoc' "of"
commaPandocOr English = commaEngPandoc' "or"

quadsOfRules :: Options -> [Rule] -> [Quad]
quadsOfRules opts rules 
  = makeAllQuads (converse [ (conj, rc_orgRules conj) | conj <- makeAllConjs opts rules ])

        -- Quads embody the "switchboard" of rules. A quad represents a "proto-rule" with the following meaning:
        -- whenever relation r is affected (i.e. tuples in r are inserted or deleted),
        -- the rule may have to be restored using functionality from one of the clauses.
makeAllQuads :: [(Rule, [Conjunct])] -> [Quad]
makeAllQuads conjsPerRule =
  [ Quad { qDcl     = d
         , qRule    = rule
         , qConjuncts = conjs
         }
  | (rule,conjs) <- conjsPerRule, d <-relsUsedIn rule
  ]
  
   