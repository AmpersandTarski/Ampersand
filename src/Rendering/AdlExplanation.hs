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
  explain :: Fspc -> Options -> a -> [Explanation]
  explain fSpec flags x = [e | e<-fSexpls fSpec++ map (autoExpl2Explain x) (autoExplainsOf flags x) 
                             , explForObj x e
                             , language flags == explLang e
                          ]
  autoExpl2Explain :: a -> AutoExplain -> Explanation
  explForObj :: a -> Explanation -> Bool
  autoExplainsOf :: Options -> a -> [AutoExplain]
  
instance Explainable ConceptDef where
  autoExplainsOf _ _ = []
  explForObj cd ExplConcept{explObjCD = cd'} = cd == cd'
  explForObj _ _ = False
  autoExpl2Explain cd (Because lang ec) = 
               ExplConcept{explObjCD = cd
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}          

instance Explainable Concept where
  autoExplainsOf _ _ = []
  explForObj c ExplConcept{explObjCD = cd} = name c == name cd
  explForObj _ _ = False
--  autoExpl2Explain c (Because lang ec)   moet nog gemaakt worden... (SJ: 24 aug 2010)          

instance Explainable Declaration where
  autoExplainsOf flags decl = autoExplain flags decl
  explForObj decl ExplDeclaration{explObjD = decl'} = decl == decl'
  explForObj _ _ = False
  autoExpl2Explain decl (Because lang ec) = 
           ExplDeclaration{explObjD  = decl
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}
  
instance Explainable Rule where
  autoExplainsOf flags rule = autoExplain flags rule
  explForObj rule ExplRule   {explObjR = rule'} = rule == rule'
  explForObj _ _ = False
  autoExpl2Explain rule (Because lang ec) = 
                 ExplRule {explObjR  = rule
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}
  
instance Explainable KeyDef where
  autoExplainsOf _ _ = []
  explForObj kd ExplKeyDef {explObjKD = kd'} = kd ==kd'
  explForObj _ _ = False
  autoExpl2Explain kd (Because lang ec) =
               ExplKeyDef {explObjKD = kd
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}

instance Explainable ObjectDef where
  autoExplainsOf _ _ = []
  explForObj od ExplObjectDef{explObjOD = od'} = od ==od'
  explForObj _ _ = False
  autoExpl2Explain od (Because lang ec) =
             ExplObjectDef{explObjOD = od
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}

instance Explainable Pattern where
  autoExplainsOf _ _ = []
  explForObj pat e@ExplPattern{} = (name pat ==name e)
  explForObj _ _ = False
  autoExpl2Explain pat (Because lang ec) =
              ExplPattern {explObjP  = name pat
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}

instance Explainable Context where
  autoExplainsOf _ _ = []
  explForObj ctx e@ExplContext{} = (name ctx == name e)
  explForObj _ _ = False
  autoExpl2Explain ctx (Because lang ec) =
              ExplContext {explObjC  = name ctx
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}

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