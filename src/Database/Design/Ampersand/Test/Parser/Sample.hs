{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.Sample (runTests) where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Data.Char
import Control.Applicative

import Database.Design.Ampersand.ADL1.P2A_Converters(Guarded(..),pCtx2aCtx)
import Database.Design.Ampersand.Core.AbstractSyntaxTree (A_Context(..))
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.Input.ADL1.UU_Scanner
import Database.Design.Ampersand.InputProcessing
import Database.Design.Ampersand.Misc.Options
import Debug.Trace

options :: Options
options = Options {}

noCtx :: A_Context
noCtx = ACtx{ctxnm="NO_CONTEXT"}

parse :: String -> A_Context
parse str = let pResult = runParser pContext "Sample.hs" str
            in case pResult of
               Errors err          -> trace ("\nCannot parse '" ++ str ++ "': " ++ show err) noCtx
               Checked (pctx,_)    ->
                    let aResult = pCtx2aCtx options pctx
                    in case aResult of
                       Errors err          -> trace ("\nType error in '" ++ str ++ "': " ++ show err) noCtx
                       Checked actx -> actx

instance Arbitrary A_Context where
    arbitrary = ACtx <$> identifier              -- ^ The name of this context
         <*> elements [[]] -- [Origin]           -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         <*> elements [English,Dutch]            -- ^ The default language used in this context.
         <*> elements [HTML,ReST,LaTeX,Markdown] -- ^ The default markup format for free text in this context (currently: LaTeX, ...)
         <*> arbitrary                           -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         <*> elements [[]] -- [Pattern]        -- ^ The patterns defined in this context
         <*> elements [[]] -- [Process]        -- ^ The processes defined in this context
         <*> elements [[]] -- [Rule]           -- ^ All user defined rules in this context, but outside patterns and outside processes
         <*> elements [[]] -- [Declaration]    -- ^ The relations that are declared in this context, outside the scope of patterns
         <*> elements [[]] -- [Population]     -- ^ The user defined populations of relations defined in this context, including those from patterns and processes
         <*> elements [[]] -- [ConceptDef]     -- ^ The concept definitions defined in this context, including those from patterns and processes
         <*> elements [[]] -- [IdentityDef]    -- ^ The identity definitions defined in this context, outside the scope of patterns
         <*> elements [[]] -- [ViewDef]        -- ^ The view definitions defined in this context, outside the scope of patterns
         <*> elements [[]] -- [A_Gen]          -- ^ The specialization statements defined in this context, outside the scope of patterns
         <*> elements [[]] -- [[A_Concept]]    -- ^ A partitioning of all concepts: the union of all these concepts contains all atoms, and the concept-lists are mutually distinct in terms of atoms in one of the mentioned concepts
         <*> elements [[]] -- [Interface]      -- ^ The interfaces defined in this context
         <*> elements [[]] -- [Purpose]        -- ^ The purposes of objects defined in this context, outside the scope of patterns and processes
         <*> elements [[]] -- [ObjectDef]      -- ^ user defined sqlplugs, taken from the Ampersand script
         <*> elements [[]] -- [ObjectDef]      -- ^ user defined phpplugs, taken from the Ampersand script
         <*> elements [[]] -- [Meta]           -- ^ used for Pandoc authors (and possibly other things)
       where identifier = suchThat (listOf1 (elements (['a'..'z']++['A'..'Z']++['0'..'9']++"_"))) (\a -> isUpper (head a) && length a > 1)

prop_pretty :: A_Context -> Bool
prop_pretty xs =
            if xs == p then True
            else trace ("original:" ++ (showADL xs) ++ "parsed:" ++ (showADL p)) False
            where p = parse (showADL xs)

runTests :: IO ()
runTests = do quickCheck prop_pretty
              --- quickCheck prop_2
-- main = $quickCheckAll >>= print
