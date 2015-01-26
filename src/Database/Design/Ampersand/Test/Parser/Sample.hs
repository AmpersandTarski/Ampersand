{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction #-}

module Database.Design.Ampersand.Test.Parser.Sample (runTests) where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Database.Design.Ampersand.Input.ADL1.UU_Scanner
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.InputProcessing
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Misc.Options
import Database.Design.Ampersand.ADL1.P2A_Converters(Guarded(..),pCtx2aCtx)
import Database.Design.Ampersand.Core.AbstractSyntaxTree (A_Context(..))
import Debug.Trace

options :: Options
options = Options {}

parse :: String -> A_Context
parse str = let pResult = runParser pContext str "Sample.hs"
            in case pResult of
               Errors _            -> trace ("Cannot parse " ++ str) ACtx{}
               Checked (pctx,incl) ->
                    let aResult = pCtx2aCtx options pctx
                    in case aResult of
                       Errors _            -> trace ("Type error " ++ str) ACtx{}
                       Checked actx -> actx

-- prop_simple = parse "CONTEXT SelectExprTest IN ENGLISH"
{-data A_Context
   = ACtx{ ctxnm :: String           -- ^ The name of this context
         , ctxpos :: [Origin]        -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         , ctxlang :: Lang           -- ^ The default language used in this context.
         , ctxmarkup :: PandocFormat -- ^ The default markup format for free text in this context (currently: LaTeX, ...)
         , ctxthms :: [String]       -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         , ctxpats :: [Pattern]      -- ^ The patterns defined in this context
         , ctxprocs :: [Process]     -- ^ The processes defined in this context
         , ctxrs :: [Rule]           -- ^ All user defined rules in this context, but outside patterns and outside processes
         , ctxds :: [Declaration]    -- ^ The relations that are declared in this context, outside the scope of patterns
         , ctxpopus :: [Population]  -- ^ The user defined populations of relations defined in this context, including those from patterns and processes
         , ctxcds :: [ConceptDef]    -- ^ The concept definitions defined in this context, including those from patterns and processes
         , ctxks :: [IdentityDef]    -- ^ The identity definitions defined in this context, outside the scope of patterns
         , ctxvs :: [ViewDef]        -- ^ The view definitions defined in this context, outside the scope of patterns
         , ctxgs :: [A_Gen]          -- ^ The specialization statements defined in this context, outside the scope of patterns
         , ctxgenconcs :: [[A_Concept]] -- ^ A partitioning of all concepts: the union of all these concepts contains all atoms, and the concept-lists are mutually distinct in terms of atoms in one of the mentioned concepts
         , ctxifcs :: [Interface]    -- ^ The interfaces defined in this context
         , ctxps :: [Purpose]        -- ^ The purposes of objects defined in this context, outside the scope of patterns and processes
         , ctxsql :: [ObjectDef]     -- ^ user defined sqlplugs, taken from the Ampersand script
         , ctxphp :: [ObjectDef]     -- ^ user defined phpplugs, taken from the Ampersand script
         , ctxmetas :: [Meta]        -- ^ used for Pandoc authors (and possibly other things)
         }               --deriving (Show) -- voor debugging
-}
instance Arbitrary A_Context where
    arbitrary = return ACtx{}

prop_pretty :: A_Context -> Bool
prop_pretty xs = parse (showADL xs) == xs
  where types = xs::A_Context

runTests :: IO ()
runTests = quickCheck prop_pretty
-- main = $quickCheckAll >>= print
