{-# OPTIONS_GHC -Wall #-}
--DESCR -> functions translating adl to natural language.
--TODO -> Maybe this module is useful at more places than just func spec rendering. In that case it's not a Rendering module and it needs to be replaced
module Rendering.AdlExplanation(explain,ExplainOutputFormat(..),explain2Blocks,format) where
import Adl hiding (applyM)
import Data.Fspec
import Options
import Data.Explain
import Data.List
import Strings          (unCap,upCap,preciesEen)
import Version (versionbanner)
import Languages        (Lang(..),plural)
import Collection       (Collection ((>-)))
import Text.Pandoc
import Char             (toLower)
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

instance Explainable ObjectDef where
  explForObj x (ExplObjectDef x') = x == x'
  explForObj _ _ = False

instance Explainable Pattern where
  explForObj x (ExplPattern str) = name x == str
  explForObj _ _ = False

instance Explainable Context where
  explForObj x (ExplContext str) = name x == str
  explForObj _ _ = False

data ExplainOutputFormat = PlainText 
format  :: ExplainOutputFormat -> [Explanation] -> String
format fmtType expls
   = intercalate "\n" (map (formatOne fmtType) expls)
   where formatOne ::  ExplainOutputFormat -> Explanation -> String
         formatOne PlainText expl = explainContent2String (explCont expl)

explain2Blocks :: Explanation -> [Block]
explain2Blocks e = explainContent2Blocks (explCont e)


class SelfExplained a where
    -- TODO: Samenvoegen met Explained
    autoExplains :: Options -> a -> [AutoExplain]  -- List of inner (generated) explanations of the object (like Rule, Morphism, ..)

instance SelfExplained Declaration where
     autoExplains flags d = [explainParagraph flags{language=Dutch}   dutchInlines] 
                        ++  [explainParagraph flags{language=English} englishInlines]
      where dutchInlines 
                 | null ([Sym,Asy]         >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is een eigenschap van "]
                                                                ++[Str ((unCap.plural Dutch .name.source) d)]
                                                                ++[Str "."]
		         | null ([Sym,Rfx,Trn]     >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is een equivalentierelatie tussen "]
                                                                ++[Str ((unCap.plural Dutch .name.source) d)]
                                                                ++[Str "."]
		         | null ([Asy,Trn]         >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is een ordeningsrelatie tussen "]
                                                                ++[Str ((unCap.plural Dutch .name.source) d)]
                                                                ++[Str "."]
		         | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d)
		                                                                   ("precies "++preciesEen++" "++(unCap.name.target) d)
		                                                        ++[Str " en vice versa."]
		         | null ([Uni,Tot,Inj]     >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d)
		                                                                   ("precies "++preciesEen++" "++(unCap.name.target) d)
		                                                        ++[Str ", maar niet voor elke "]
		                                                        ++[Str ((unCap.name.target) d)]
		                                                        ++[Str (" hoeft er een "++(unCap.name.source) d++" te zijn.")]
		         | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d)
		                                                                   ("precies "++preciesEen++" "++(unCap.name.target) d)
		                                                        ++[Str (", maar elke "++(unCap.name.target) d++" is gerelateerd aan "++preciesEen++" of meer "++(unCap.plural Dutch .name.source) d++".")]
		         | null ([Uni,    Inj,Sur] >- multiplicities d) = [Str ("Er is precies "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: " )]
		                                                        ++applyM d "b" "a"
		                                                        ++[Str (", maar niet voor elke "++(unCap.name.source) d++" hoeft er een "++(unCap.name.target) d++" te zijn.")]
		         | null ([    Tot,Inj,Sur] >- multiplicities d) = [Str ("Er is precies "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: ")]
		                                                        ++applyM d "b" "a"
		                                                        ++[Str (", maar elke "++(unCap.name.source) d++" mag gerelateerd zijn aan meerdere "++(unCap.plural Dutch .name.target) d++".")]
		         | null ([Uni,Tot        ] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d)
		                                                                   ("precies "++preciesEen++" "++(unCap.name.target) d)
		                                                        ++[Str "."]
		         | null ([Uni,    Inj    ] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d)
		                                                                   ("ten hoogste "++preciesEen++" "++(unCap.name.target) d)
		                                                        ++[Str (" en elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste "++preciesEen++" "++(unCap.name.source) d++".")]
		         | null ([Uni,        Sur] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d)
		                                                                   ("ten hoogste "++preciesEen++" "++(unCap.name.target) d)
		                                                        ++[Str (", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan tenminste "++preciesEen++" "++(unCap.name.source) d++".")]
		         | null ([    Tot,Inj    ] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d)
		                                                                   ("tenminste "++preciesEen++" "++(unCap.name.target) d)
		                                                        ++[Str (", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste "++preciesEen++" "++(unCap.name.source) d++".")]
		         | null ([    Tot,    Sur] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d)
		                                                                   ("tenminste "++preciesEen++" "++(unCap.name.target) d)
		                                                        ++[Str (" en elke "++(unCap.name.target) d++" is gerelateerd aan tenminste "++preciesEen++" "++(unCap.name.source) d++".")]
		         | null ([        Inj,Sur] >- multiplicities d) = [Str ("Er is precies "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: ")]
		                                                        ++applyM d "b" "a"
		                                                        ++[Str "."]
		         | null ([            Sur] >- multiplicities d) = [Str ("Er is tenminste "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: ")]
		                                                        ++applyM d "b" "a"
		                                                        ++[Str "."]
		         | null ([        Inj    ] >- multiplicities d) = [Str ("Er is hooguit "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: ")]
		                                                        ++applyM d "b" "a"
		                                                        ++[Str "."]
		         | null ([    Tot        ] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d)
		                                                                   ("tenminste "++preciesEen++" "++(unCap.name.target) d)
		                                                        ++[Str "."]
		         | null ([Uni            ] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d)
		                                                                   ("nul of "++preciesEen++" "++(unCap.name.target) d)
		                                                        ++[Str "."]
		         | otherwise                                    = [Str "De zin: "]
		                                                        ++[Quoted DoubleQuote 
		                                                            (applyM d ((var [].source) d)
		                                                                      ((var [source d].target) d))
		                                                          ]
		                                                        ++[Str (" heeft betekenis (dus: is waar of niet waar) voor een "++(unCap.name.source) d++" ")]
		                                                        ++[Str ((var [].source) d)]
		                                                        ++[Str (" en een "++(unCap.name.target) d++" ")]
		                                                        ++[Str ((var [source d].target) d)]
		                                                        ++[Str "."]
		                                                          
            englishInlines 
		         | null ([Sym,Asy]         >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is a property of "]
                                                                ++[Str ((unCap.plural English .name.source) d)]
                                                                ++[Str "."]
		         | null ([Sym,Rfx,Trn]     >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is an equivalence relation on "]
                                                                ++[Str ((unCap.plural English .name.source) d)]
                                                                ++[Str "."]
		         | null ([Asy,Trn]         >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is an ordering relation on "]
                                                                ++[Str ((unCap.plural English .name.source) d)]
                                                                ++[Str "."]
		         | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM d ("each "++(unCap.name.source) d)
		                                                                   ("exactly one "++(unCap.name.target) d)
		                                                        ++[Str " and vice versa."]
		         | null ([Uni,Tot,Inj    ] >- multiplicities d) = applyM d ("each "++(unCap.name.source) d)
		                                                                   ("exactly one "++(unCap.name.target) d)
		                                                        ++[Str ", but not for each "]
		                                                        ++[Str ((unCap.name.target) d++" there must be a "++(unCap.name.source) d)]
		                                                        ++[Str "."]
		         | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM d ("each "++(unCap.name.source) d)
		                                                                   ("exactly one "++(unCap.name.target) d)
		                                                        ++[Str ", but each "]
		                                                        ++[Str ((unCap.name.target) d++" is related to one or more "++(unCap.plural English .name.source) d)]
		                                                        ++[Str "."]
		         | null ([Uni,    Inj,Sur] >- multiplicities d) = [Str ("There is exactly one "++(unCap.name.source) d++" (a) for each "++(unCap.name.target) d++" (b), for which: ")]
		                                                        ++applyM d "b" "a"
		                                                        ++[Str (", but not for each "++(unCap.name.source) d++" there must be a "++(unCap.name.target) d++".")]
		         | null ([    Tot,Inj,Sur] >- multiplicities d) = [Str ("There is exactly one "++(unCap.name.source) d++" (a) for each "++(unCap.name.target) d++" (b), for which: ")]
		                                                        ++applyM d "b" "a"
		                                                        ++[Str (", but each "++(unCap.name.source) d++" is related to one or more "++(unCap.plural English .name.target) d)]
		                                                        ++[Str "."]
		         | null ([Uni,Tot        ] >- multiplicities d) = applyM d ("each "++(unCap.name.source) d)
		                                                                   ("exactly one "++(unCap.name.target) d)
		                                                        ++[Str "."]
		         | null ([Uni,    Inj    ] >- multiplicities d) = applyM d ("each "++(unCap.name.source) d)
		                                                                   ("at most one "++(unCap.name.target) d)
		                                                        ++[Str (" and each "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d)]
		                                                        ++[Str "."]
		         | null ([Uni,        Sur] >- multiplicities d) = applyM d ("each "++(unCap.name.source) d)
		                                                                   ("at most one "++(unCap.name.target) d)
		                                                        ++[Str (", whereas each "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d)]
		                                                        ++[Str "."]
		         | null ([    Tot,Inj    ] >- multiplicities d) = applyM d ("each "++(unCap.name.source) d)
		                                                                   ("at least one "++(unCap.name.target) d)
		                                                        ++[Str (", whereas each "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d)]
		                                                        ++[Str "."]
		         | null ([    Tot,    Sur] >- multiplicities d) = applyM d ("each "++(unCap.name.source) d)
		                                                                   ("at least one "++(unCap.name.target) d)
		                                                        ++[Str (" and each "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d)]
		                                                        ++[Str "."]
		         | null ([        Inj,Sur] >- multiplicities d) = [Str ("There is exactly one "++(unCap.name.source) d++" (a) for each "++(unCap.name.target) d++" (b), for which: ")]
		                                                        ++applyM d "b" "a"
		                                                        ++[Str "."]
		         | null ([            Sur] >- multiplicities d) = [Str ("There is at least one "++(unCap.name.source) d++" (a) for each "++(unCap.name.target) d++" (b), for which: ")]
		                                                        ++applyM d "b" "a"
		                                                        ++[Str "."]
		         | null ([        Inj    ] >- multiplicities d) = [Str ("There is at most one "++(unCap.name.source) d++" (a) for each "++(unCap.name.target) d++" (b), for which: ")]
		                                                        ++applyM d "b" "a"
		                                                        ++[Str "."]
		         | null ([    Tot        ] >- multiplicities d) = applyM d ("each "++(unCap.name.source) d)
		                                                                   ("at least one "++(unCap.name.target) d)
		                                                        ++[Str "."]
		         | null ([Uni            ] >- multiplicities d) = applyM d ("each "++(unCap.name.source) d)
		                                                                   ("zero or one "++(unCap.name.target) d)
		                                                        ++[Str "."]
		         | otherwise                                    = [Str "The sentence: "]
		                                                        ++[Quoted DoubleQuote 
		                                                            (applyM d ((var [].source) d)
		                                                                      ((var [source d].target) d))
		                                                          ]
		                                                        ++[Str (" is meaningful (i.e. it is either true or false) for any "++(unCap.name.source) d++" ")]
		                                                        ++[Str ((var [].source) d)]
		                                                        ++[Str (" and "++(unCap.name.target) d++" ")]
		                                                        ++[Str ((var [source d].target) d)]
		                                                        ++[Str "."]
            applyM :: Declaration -> String -> String -> [Inline]
            applyM decl a b =
               case decl of
                 Sgn{}     -> if null (prL++prM++prR) 
                                then [Str (upCap a)]++[Str (" corresponds to ")]++[Str b ]++[Str (" in relation "++decnm decl)]
                                else [Str (upCap str)| let str=prL++" "++a++" "++prM++" "++b++" "++prR]
                              where prL = decprL decl
                                    prM = decprM decl
                                    prR = decprR decl
                 Isn{}     -> [Str a]++[Str " equals "]++[Str b]
                 Iscompl{} -> [Str a]++[Str " differs from "]++[Str b]
                 Vs{}      -> [Str (show True)]
            
            var :: Identified a => [a] -> a -> String     -- TODO Vervangen door mkvar, uit predLogic.hs
            var seen c = low c ++ ['\''| c'<-seen, low c == low c']
                      where low idt= if null (name idt) then "x" else [(toLower.head.name) idt]
            
            
instance SelfExplained Rule where
    autoExplains _ r = rrxpl r         -- TODO: to allow explainations in multiple languages, change to:  explain options d@Sgn{} = etc...
            