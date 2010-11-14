{-# OPTIONS_GHC -Wall #-}
--DESCR -> functions translating adl to natural language.
--TODO -> Maybe this module is useful at more places than just func spec rendering. In that case it's not a Rendering module and it needs to be replaced
module Rendering.AdlExplanation(explain,ExplainOutputFormat(..),explain2Blocks,format) where
import Adl
import Data.Fspec
import Options
import Data.Explain
import Strings (chain)
import Version (versionbanner)
import Text.Pandoc
--instance Explained Expression where
--    explain flags e = showPredLogic flags e

-- The general idea is that an ADL declaration such as:
--     EXPLAIN r[A*B] IN ENGLISH
--     {+ This text explains why r[A*B] exists -}
-- produces the exact right text in the functional specification


-- The class Explainable exists so that we can write the Haskell expression 'explain fSpec flags x' anywhere we like for every
-- type of x that could possibly be motivated in an Explanation.
-- 'explain fSpec flags x' produces all explanations related to x from the context (fSpec) that are available in the language specified in 'flags'.
-- The other functions in this class are solely meant to be used in the definition of explain.
-- They are defined once for each instance of Explainable, not be used in other code.
-- TODO: Han, kan dat worden afgeschermd, zodat de programmeur alleen 'explain' ziet en de andere functies dus niet kan gebruiken?
--     @Stef: Ja, het is al zoveel mogelijk afgeschermd (zie definities die deze module exporteert, hierboven) maar er wordt nog gebruik van gemaakt voor oa foutmeldingen in de atlas, en het prototype. Zodra iemand iets anders verzint voor het gebruik van "ExplainOutputFormat(..),explain2Blocks,format", kunnen deze uit de export-list van deze module worden verwijderd.
class Explainable a where 
  autoExplainsOf :: Options -> a -> [Explanation]
  autoExplainsOf _ _ = []
  explain :: Fspc -> Options -> a -> [Explanation]
  explain fSpec flags x = [e | e<-fSexpls fSpec++autoExplainsOf flags x 
                             , explForObj x (explObj e)
                             , language flags == explLang e
                          ]
  explForObj :: a -> ExplObj -> Bool          -- Given an Explainable object and an ExplObj, return TRUE if and only if there is a match.
  
instance Explainable ConceptDef where
  explForObj x (ExplConceptDef x') = x == x'
  explForObj _ _ = False

instance Explainable Concept where
  explForObj x (ExplConceptDef x') = name x == name x'
  explForObj _ _ = False

instance Explainable Declaration where
  autoExplainsOf flags decl = map (toExpl decl) (autoExplains flags decl)
     where 
       toExpl :: Declaration -> AutoExplain -> Explanation
       toExpl d (Because l econt) = Expl (ExplDeclaration d) l versionbanner econt
  explForObj x (ExplDeclaration x') = x == x'
  explForObj _ _ = False
  
instance Explainable Rule where
  autoExplainsOf flags rule = map (toExpl rule) (autoExplains flags rule)
     where
        toExpl :: Rule -> AutoExplain -> Explanation
        toExpl r (Because l econt) = Expl ( ExplRule r) l versionbanner econt
  explForObj x (ExplRule x') = x == x'
  explForObj _ _ = False
  
instance Explainable KeyDef where
  explForObj x (ExplKeyDef x') = x == x'
  explForObj _ _ = False
--  autoExpl2Explain kd (Because lang ec) =
--               ExplKeyDef {explObjKD = kd
--                          ,explLang  = lang
--                          ,explRefId = versionbanner
--                          ,explCont  = ec}

instance Explainable ObjectDef where
  explForObj x (ExplObjectDef x') = x == x'
  explForObj _ _ = False
--  autoExpl2Explain od (Because lang ec) =
--             ExplObjectDef{explObjOD = od
--                          ,explLang  = lang
--                          ,explRefId = versionbanner
--                          ,explCont  = ec}

instance Explainable Pattern where
  explForObj x (ExplPattern str) = name x == str
  explForObj _ _ = False
--  autoExpl2Explain pat (Because lang ec) =
--              ExplPattern {explObjP  = name pat
--                          ,explLang  = lang
--                          ,explRefId = versionbanner
--                          ,explCont  = ec}

instance Explainable Context where
  explForObj x (ExplContext str) = name x == str
  explForObj _ _ = False
--  autoExpl2Explain ctx' (Because lang ec) =
--              ExplContext {explObjC  = name ctx'
--                          ,explLang  = lang
--                          ,explRefId = versionbanner
--                          ,explCont  = ec}

data ExplainOutputFormat = PlainText 
format  :: ExplainOutputFormat -> [Explanation] -> String
format fmtType expls
 = chain "\n" (map (formatOne fmtType) expls)
 where formatOne ::  ExplainOutputFormat -> Explanation -> String
       formatOne PlainText expl = explainContent2String (explCont expl)

explain2Blocks :: Explanation -> [Block]
explain2Blocks e = explainContent2Blocks (explCont e)
--   case expls of
--      []   -> []
--      e:es -> explainContent2Inlines (explCont e) 
--              ++ [Str " "]++ explains2Inlines es
--       