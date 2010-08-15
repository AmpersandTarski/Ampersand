{-# OPTIONS_GHC -Wall #-}
--DESCR -> functions translating adl to natural language.
--TODO -> Maybe this module is useful at more places than just func spec rendering. In that case it's not a Rendering module and it needs to be replaced
module Rendering.AdlExplanation(explain,ExplainOutputFormat(..),format) where
import Adl
import Data.Fspec
import Options
import Data.Explain
import Strings (chain)
import Version (versionbanner)
--instance Explained Expression where
--    explain flags e = showPredLogic flags e

-- The general idea is that an ADL declaration such as:
-- EXPLAIN r[A*B] IN ENGLISH
-- {+ This text explains why r[A*B] exists -}
-- produces the exact right text in the functional specification
--explainDecl :: Options -> Fspc -> Declaration -> [Explanation]
--explainDecl flags fSpec d = filterExplanations d (fSexpls fSpec) flags
--  = chain "\n" ([expl| expl<-expls]++[explainMult options fSpec d])
--    where expls = [expl| ExplDeclaration d' lang _ expl<-explanations fSpec, not (null expl), d'==d, lang==language options]

class Explainable a where 
  explain :: Fspc -> Options -> a -> [Explanation]
  explain fSpec flags x = 
        filterExplanations x flags (fSexpls fSpec++ map (autoExpl2Explain x) (autoExplainsOf x)) 
  filterExplanations :: a             -- the object the filter is for 
                     -> Options       -- filter by language, and maybe later more.
                     -> [Explanation] -- A list of all explanations (most likely from the Fspec)
                     -> [Explanation] -- the relevant explanations
  filterExplanations a flags expls = [e | e<-expls 
                                        , explForObj a e
                                        , explForLang (language flags) e
                                     ]
     where explForLang lang e = lang == explLang e
  autoExpl2Explain :: a -> AutoExplain -> Explanation
  explForObj :: a -> Explanation -> Bool
  autoExplainsOf :: a -> [AutoExplain]
  
instance Explainable ConceptDef where
  autoExplainsOf _ = []
  explForObj cd ExplConcept{explObjCD = cd'} = cd == cd'
  explForObj _ _ = False
  autoExpl2Explain cd (Because lang ec) = 
               ExplConcept{explObjCD = cd
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}          

instance Explainable Declaration where
  autoExplainsOf decl = autoExplain decl
  explForObj decl ExplDeclaration{explObjD = decl'} = decl == decl'
  explForObj _ _ = False
  autoExpl2Explain decl (Because lang ec) = 
           ExplDeclaration{explObjD  = decl
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}
  
instance Explainable Rule where
  autoExplainsOf rule = autoExplain rule
  explForObj rule ExplRule   {explObjR = rule'} = rule == rule'
  explForObj _ _ = False
  autoExpl2Explain rule (Because lang ec) = 
                 ExplRule {explObjR  = rule
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}
  
instance Explainable KeyDef where
  autoExplainsOf _ = []
  explForObj kd ExplKeyDef {explObjKD = kd'} = kd ==kd'
  explForObj _ _ = False
  autoExpl2Explain kd (Because lang ec) =
               ExplKeyDef {explObjKD = kd
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}

instance Explainable ObjectDef where
  autoExplainsOf _ = []
  explForObj od ExplObjectDef{explObjOD = od'} = od ==od'
  explForObj _ _ = False
  autoExpl2Explain od (Because lang ec) =
             ExplObjectDef{explObjOD = od
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}

instance Explainable Pattern where
  autoExplainsOf _ = []
  explForObj pat ExplPattern {explObjP = patname} = name pat ==patname
  explForObj _ _ = False
  autoExpl2Explain pat (Because lang ec) =
              ExplPattern {explObjP  = name pat
                          ,explLang  = lang
                          ,explRefId = versionbanner
                          ,explCont  = ec}

--explainRule :: Options -> Fspc -> Rule -> [Explanation]
--explainRule flags fSpec r
--  = if null relevants
--    then [ExplRule r "ADL-generated" (artExpls English "Artificial explanation: ")]
--      ++ [ExplRule r "ADL-gegenereerd" (artExpls Dutch "Kunstmatige uitleg: ")]
--    else relevants ++ [ExplRule r "" (rrxpl r)]
--      where
--        artExpls :: Lang -> String -> ExplainContents
--        artExpls lang str = string2Explain lang (str ++ showPredLogic flags{language=lang} r) 
--        l = language flags
--        relevants = filterExplanations r (explanations fSpec) flags

data ExplainOutputFormat = PlainText 
format  :: ExplainOutputFormat -> [Explanation] -> String
format fmtType expls
 = chain "\n" (map (formatOne fmtType) expls)
 where formatOne ::  ExplainOutputFormat -> Explanation -> String
       formatOne PlainText expl = explainContent2String (explCont expl)
       