{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.Sample (runTests) where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
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
               Checked (pctx,incl) ->
                    let aResult = pCtx2aCtx options pctx
                    in case aResult of
                       Errors err          -> trace ("\nType error in '" ++ str ++ "': " ++ show err) noCtx
                       Checked actx -> actx

instance Arbitrary A_Context where
    arbitrary = do
       NonNegative (x::Int) <- arbitrary
       return $ ACtx {
           ctxnm = "Context" ++ show x    -- ^ The name of this context
         , ctxpos = [] -- [Origin]        -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         , ctxlang = English              -- ^ The default language used in this context.
         , ctxmarkup = LaTeX              -- ^ The default markup format for free text in this context (currently: LaTeX, ...)
         , ctxthms = [] -- [String]       -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         , ctxpats = [] -- [Pattern]      -- ^ The patterns defined in this context
         , ctxprocs = [] -- [Process]     -- ^ The processes defined in this context
         , ctxrs = [] -- [Rule]           -- ^ All user defined rules in this context, but outside patterns and outside processes
         , ctxds = [] -- [Declaration]    -- ^ The relations that are declared in this context, outside the scope of patterns
         , ctxpopus = [] -- [Population]  -- ^ The user defined populations of relations defined in this context, including those from patterns and processes
         , ctxcds = [] -- [ConceptDef]    -- ^ The concept definitions defined in this context, including those from patterns and processes
         , ctxks = [] -- [IdentityDef]    -- ^ The identity definitions defined in this context, outside the scope of patterns
         , ctxvs = [] -- [ViewDef]        -- ^ The view definitions defined in this context, outside the scope of patterns
         , ctxgs = [] -- [A_Gen]          -- ^ The specialization statements defined in this context, outside the scope of patterns
         , ctxgenconcs = [] -- [[A_Concept]] -- ^ A partitioning of all concepts: the union of all these concepts contains all atoms, and the concept-lists are mutually distinct in terms of atoms in one of the mentioned concepts
         , ctxifcs = [] -- [Interface]    -- ^ The interfaces defined in this context
         , ctxps = [] -- [Purpose]        -- ^ The purposes of objects defined in this context, outside the scope of patterns and processes
         , ctxsql = [] -- [ObjectDef]     -- ^ user defined sqlplugs, taken from the Ampersand script
         , ctxphp = [] -- [ObjectDef]     -- ^ user defined phpplugs, taken from the Ampersand script
         , ctxmetas = [] -- [Meta]        -- ^ used for Pandoc authors (and possibly other things)
         }

prop_pretty :: A_Context -> Bool
prop_pretty xs = original == parsed
  where types = xs::A_Context
        original = trace ("original:" ++ (showADL xs)) xs
        parsed = trace ("parsed:" ++ (showADL p)) p
        p = parse (showADL xs)

runTests :: IO ()
runTests = do quickCheck prop_pretty
-- main = $quickCheckAll >>= print
