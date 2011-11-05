{-# OPTIONS_GHC -Wall #-}
--TODO -> Maybe this module is useful at more places than just func spec rendering.
--        In that case it's not a Rendering module and it needs to be replaced
module DatabaseDesign.Ampersand.Output.AdlExplanation(Explainable(purpose,meaning,explanations,autoMeaning)) where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Fspec.Fspec(FProcess(..)) -- TODO FProc should not be in here at the first place... It has been put here because of the removal of Activities from Process
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Basics
import Text.Pandoc
import DatabaseDesign.Ampersand.Output.PredLogic
import Data.Char             (toLower)

fatal :: Int -> String -> a
fatal = fatalMsg "Output.AdlExplanation"


-- The general idea is that an Ampersand declaration such as:
--     EXPLAIN r[A*B] IN ENGLISH
--     {+ This text explains why r[A*B] exists -}
-- produces the exact right text in the functional specification

-- The class Explainable exists so that we can write the Haskell expression 'purpose fSpec l x'
-- anywhere we like for every type of x that could possibly be motivated in an Explanation.
-- 'purpose fSpec l x' produces all explanations related to x from the context (fSpec)
--  that are available in the language specified in 'l'.
-- The other functions in this class are solely meant to be used in the definition of purpose.
-- They are defined once for each instance of Explainable, not be used in other code.
-- TODO: Han, kan dat worden afgeschermd, zodat de programmeur alleen 'purpose' ziet en de andere functies
--       dus niet kan gebruiken?
--     @Stef: Ja, het is al zoveel mogelijk afgeschermd (zie definities die deze module exporteert, hierboven)
--     maar er wordt nog gebruik van gemaakt voor oa foutmeldingen in de atlas, en het prototype.
--     Zodra iemand iets anders verzint voor het gebruik van "ExplainOutputFormat(..),format",
--     kunnen deze uit de export-list van deze module worden verwijderd.
class Explainable a where 
  meaning :: Lang -> a -> [Block]               -- ^ explains the meaning of a. The meaning is preferrably specified by the user. If not, Ampersand tries to generate a meaning in natural language.
  purpose :: Fspc -> Lang -> a -> [Explanation] -- ^ explains the purpose of a, i.e. the reason why a exists.
  purpose fSpec l x = expls
   where expls = [e | e<-explanations fSpec
                    , explForObj x (explObj e)                  -- informally: "if x and e are the same"
                    , l == explLang e
                 ]
  explForObj     :: a -> ExplObj -> Bool    -- ^ Given an Explainable object and an ExplObj, return TRUE if they concern the identical object.
  explanations   :: a -> [Explanation]  -- ^ The explanations that are defined inside a (including that of a itself)
  autoMeaning    :: Lang -> a -> [Explanation]  -- ^ List of generated explanations of the object (like Rule, Relation Concept, ..)
  autoMeaning _ _ = []
  
instance Explainable ConceptDef where
  meaning _ cd = fatal 49 ("Concept definitions have no intrinsic meaning, (used with concept definition of '"++name cd++"')")
  explForObj x (ExplConceptDef x') = x == x'
  explForObj _ _ = False
  explanations _ = []
  
instance Explainable A_Concept where
  meaning _ c = fatal 54 ("Concepts have no intrinsic meaning, (used with concept '"++name c++"')")
  explForObj x (ExplConceptDef cd) = name x == name cd
  explForObj _ _ = False
  explanations _ = []

instance Explainable Relation where
  meaning l r = meaning l (makeDeclaration r)
  explForObj r (ExplDeclaration d) = makeDeclaration r == d
  explForObj _ _ = False
  explanations _ = []
  autoMeaning l r = autoMeaning l (makeDeclaration r)

instance Explainable Declaration where
  meaning l decl = if null (decMean decl)
                   then concat [explCont expl | expl<-autoMeaning l decl, explLang expl==l]
                   else decMean decl
  explForObj _ _ = False
  explanations _ = []
  autoMeaning l d
   = [Expl { explPos   = decfpos d                       -- the position in the Ampersand script of this purpose definition
           , explObj   = ExplDeclaration d               -- The object that is explained.
           , explLang  = l                               -- The language of the explaination
           , explRefId = ""                              -- The reference of the explaination
           , explCont  = [Para langInlines]              -- The actual explanation
           } ] 
     where 
      langInlines =
       case l of
         Dutch  
             | null ([Sym,Asy]         >- multiplicities d) -> [Emph [Str (name d)]]
                                                                ++[Str " is een eigenschap van "]
                                                                ++[Str ((unCap.plural Dutch .name.source) d)]
                                                                ++[Str "."]
             | null ([Sym,Rfx,Trn]     >- multiplicities d) -> [Emph [Str (name d)]]
                                                                ++[Str " is een equivalentierelatie tussen "]
                                                                ++[Str ((unCap.plural Dutch .name.source) d)]
                                                                ++[Str "."]
             | null ([Asy,Trn]         >- multiplicities d) -> [Emph [Str (name d)]]
                                                                ++[Str " is een ordeningsrelatie tussen "]
                                                                ++[Str ((unCap.plural Dutch .name.source) d)]
                                                                ++[Str "."]
             | null ([Uni,Tot,Inj,Sur] >- multiplicities d) -> applyM d [Str ("elke "++(unCap.name.source) d)]
                                                                              [Str ("precies "++preciesEen++" "++(unCap.name.target) d)]
                                                              ++[Str " en vice versa."]
             | null ([Uni,Tot,Inj]     >- multiplicities d) -> applyM d [Str ("elke "++(unCap.name.source) d)]
                                                                         [Str ("precies "++preciesEen++" "++(unCap.name.target) d)]
                                                              ++[Str ", maar niet voor elke "]
                                                              ++[Str ((unCap.name.target) d)]
                                                              ++[Str " hoeft er een "]
                                                              ++[Str ((unCap.name.source) d)]
                                                              ++[Str " te zijn."]
             | null ([Uni,Tot,    Sur] >- multiplicities d) -> applyM d [Str ("elke "++(unCap.name.source) d)]
                                                                         [Str ("precies "++preciesEen++" "++(unCap.name.target) d)]
                                                              ++[Str ", maar elke "]
--                                                            ++[Str ((unCap.name.target) d)]
                                                              ++[Str (" is gerelateerd aan "++preciesEen++" of meer ")]
                                                              ++[Str ((unCap.plural Dutch .name.source) d)]
                                                              ++[Str "."]
              | null ([Uni,    Inj,Sur] >- multiplicities d) -> [Str ("Er is precies "++preciesEen++" "++(unCap.name.source) d++" (")]
                                                              ++[Math InlineMath "a"]
                                                              ++[Str (") voor elke "++(unCap.name.target) d++" (")]
                                                              ++[Math InlineMath "b"]
                                                              ++[Str "), waarvoor geldt: "]
                                                              ++applyM d [Math InlineMath "b"] [Math InlineMath "a"]
                                                              ++[Str (", maar niet voor elke "++(unCap.name.source) d++" hoeft er een "++(unCap.name.target) d++" te zijn.")]
              | null ([    Tot,Inj,Sur] >- multiplicities d) -> [Str ("Er is precies "++preciesEen++" "++(unCap.name.source) d++" (")]
                                                              ++[Math InlineMath "a"]
                                                              ++[Str (") voor elke "++(unCap.name.target) d++" (")]
                                                              ++[Math InlineMath "b"]
                                                              ++[Str "), waarvoor geldt: "]
                                                              ++applyM d [Math InlineMath "b"] [Math InlineMath "a"]
                                                              ++[Str (", maar elke "++(unCap.name.source) d++" mag gerelateerd zijn aan meerdere "++(unCap.plural Dutch .name.target) d++".")]
              | null ([Uni,Tot        ] >- multiplicities d) -> applyM d [Str ("elke "++(unCap.name.source) d)]
                                                                         [Str ("precies "++preciesEen++" "++(unCap.name.target) d)]
                                                              ++[Str "."]
              | null ([Uni,    Inj    ] >- multiplicities d) -> applyM d [Str ("elke "++(unCap.name.source) d)]
                                                                         [Str ("ten hoogste "++preciesEen++" "++(unCap.name.target) d)]
                                                              ++[Str " en elke "]
                                                              ++[Str ((unCap.name.target) d)]
                                                              ++[Str (" is gerelateerd aan ten hoogste "++preciesEen++" ")]
                                                              ++[Str ((unCap.name.source) d++".")]
                                                              ++[Str "."]
              | null ([Uni,        Sur] >- multiplicities d) -> applyM d [Str ("elke "++(unCap.name.source) d)]
                                                                         [Str ("ten hoogste "++preciesEen++" "++(unCap.name.target) d)]
                                                              ++[Str ", terwijl elke "]
                                                              ++[Str ((unCap.name.target) d)]
                                                              ++[Str (" is gerelateerd aan tenminste "++preciesEen++" ")]
                                                              ++[Str ((unCap.name.source) d)]
                                                              ++[Str "."]
              | null ([    Tot,Inj    ] >- multiplicities d) -> applyM d [Str ("elke "++(unCap.name.source) d)]
                                                                         [Str ("tenminste "++preciesEen++" "++(unCap.name.target) d)]
                                                              ++[Str ", terwijl elke "]
                                                              ++[Str ((unCap.name.target) d)]
                                                              ++[Str (" is gerelateerd aan ten hoogste "++preciesEen++" ")]
                                                              ++[Str ((unCap.name.source) d)]
                                                              ++[Str "."]
              | null ([    Tot,    Sur] >- multiplicities d) -> applyM d [Str ("elke "++(unCap.name.source) d)]
                                                                         [Str ("tenminste "++preciesEen++" "++(unCap.name.target) d)]
                                                              ++[Str (" en elke "++(unCap.name.target) d++" is gerelateerd aan tenminste "++preciesEen++" "++(unCap.name.source) d++".")]
              | null ([        Inj,Sur] >- multiplicities d) -> [Str ("Er is precies "++preciesEen++" "++(unCap.name.source) d++" (")]
                                                              ++[Math InlineMath "a"]
                                                              ++[Str (") voor elke "++(unCap.name.target) d++" (")]
                                                              ++[Math InlineMath "b"]
                                                              ++[Str "), waarvoor geldt: "]
                                                              ++applyM d [Math InlineMath "b"] [Math InlineMath "a"]
                                                              ++[Str "."]
              | null ([            Sur] >- multiplicities d) -> [Str ("Er is tenminste "++preciesEen++" "++(unCap.name.source) d++" (")]
                                                              ++[Math InlineMath "a"]
                                                              ++[Str (") voor elke "++(unCap.name.target) d++" (")]
                                                              ++[Math InlineMath "b"]
                                                              ++[Str "), waarvoor geldt: "]
                                                              ++applyM d [Math InlineMath "b"] [Math InlineMath "a"]
                                                              ++[Str "."]
              | null ([        Inj    ] >- multiplicities d) -> [Str ("Er is hooguit "++preciesEen++" "++(unCap.name.source) d++" (")]
                                                              ++[Math InlineMath "a"]
                                                              ++[Str (") voor elke "++(unCap.name.target) d++" (")]
                                                              ++[Math InlineMath "b"]
                                                              ++[Str "), waarvoor geldt: "]
                                                              ++applyM d [Math InlineMath "b"] [Math InlineMath "a"]
                                                              ++[Str "."]
              | null ([    Tot        ] >- multiplicities d) -> applyM d [Str ("elke "++(unCap.name.source) d)]
                                                                         [Str ("tenminste "++preciesEen++" "++(unCap.name.target) d)]
                                                              ++[Str "."]
              | null ([Uni            ] >- multiplicities d) -> applyM d [Str ("elke "++(unCap.name.source) d)]
                                                                         [Str ("nul of "++preciesEen++" "++(unCap.name.target) d)]
                                                              ++[Str "."]
              | otherwise                                    -> [Str "De zin: "]
                                                              ++[Quoted DoubleQuote 
                                                                  (applyM d [(Math InlineMath . var [] . source) d]
                                                                            [(Math InlineMath . var [source d] . target) d])
                                                                ]
                                                              ++[Str (" heeft betekenis (dus: is waar of niet waar) voor een "++(unCap.name.source) d++" ")]
                                                              ++[(Math InlineMath . var [].source) d]
                                                              ++[Str (" en een "++(unCap.name.target) d++" ")]
                                                              ++[(Math InlineMath . var [source d].target) d]
                                                              ++[Str "."]

         English 
              | null ([Sym,Asy]         >- multiplicities d) -> [Emph [Str (name d)]]
                                                              ++[Str " is a property of "]
                                                              ++[Str ((unCap.plural English .name.source) d)]
                                                              ++[Str "."]
              | null ([Sym,Rfx,Trn]     >- multiplicities d) -> [Emph [Str (name d)]]
                                                              ++[Str " is an equivalence relation on "]
                                                              ++[Str ((unCap.plural English .name.source) d)]
                                                              ++[Str "."]
              | null ([Asy,Trn]         >- multiplicities d) -> [Emph [Str (name d)]]
                                                              ++[Str " is an ordering relation on "]
                                                              ++[Str ((unCap.plural English .name.source) d)]
                                                              ++[Str "."]
              | null ([Uni,Tot,Inj,Sur] >- multiplicities d) -> applyM d [Str ("each "++(unCap.name.source) d)]
                                                                         [Str ("exactly one "++(unCap.name.target) d)]
                                                              ++[Str " and vice versa."]
              | null ([Uni,Tot,Inj    ] >- multiplicities d) -> applyM d [Str ("each "++(unCap.name.source) d)]
                                                                         [Str ("exactly one "++(unCap.name.target) d)]
                                                              ++[Str ", but not for each "]
                                                              ++[Str ((unCap.name.target) d++" there must be a "++(unCap.name.source) d)]
                                                              ++[Str "."]
              | null ([Uni,Tot,    Sur] >- multiplicities d) -> applyM d [Str ("each "++(unCap.name.source) d)]
                                                                         [Str ("exactly one "++(unCap.name.target) d)]
                                                              ++[Str ", but each "]
                                                              ++[Str ((unCap.name.target) d++" is related to one or more "++(unCap.plural English .name.source) d)]
                                                              ++[Str "."]
              | null ([Uni,    Inj,Sur] >- multiplicities d) -> [Str ("There is exactly one "++(unCap.name.source) d++" (")]
                                                              ++[Math InlineMath "a"]
                                                              ++[Str (") for each "++(unCap.name.target) d++" (")]
                                                              ++[Math InlineMath "b"]
                                                              ++[Str "), for which: "]
                                                              ++applyM d [Math InlineMath "b"] [Math InlineMath "a"]
                                                              ++[Str (", but not for each "++(unCap.name.source) d++" there must be a "++(unCap.name.target) d++".")]
              | null ([    Tot,Inj,Sur] >- multiplicities d) -> [Str ("There is exactly one "++(unCap.name.source) d++" (")]
                                                              ++[Math InlineMath "a"]
                                                              ++[Str (") for each "++(unCap.name.target) d++" (")]
                                                              ++[Math InlineMath "b"]
                                                              ++[Str "), for which: "]
                                                              ++applyM d [Math InlineMath "b"] [Math InlineMath "a"]
                                                              ++[Str (", but each "++(unCap.name.source) d++" is related to one or more "++(unCap.plural English .name.target) d)]
                                                              ++[Str "."]
              | null ([Uni,Tot        ] >- multiplicities d) -> applyM d [Str ("each "++(unCap.name.source) d)]
                                                                         [Str ("exactly one "++(unCap.name.target) d)]
                                                              ++[Str "."]
              | null ([Uni,    Inj    ] >- multiplicities d) -> applyM d [Str ("each "++(unCap.name.source) d)]
                                                                         [Str ("at most one "++(unCap.name.target) d)]
                                                              ++[Str (" and each "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d)]
                                                              ++[Str "."]
              | null ([Uni,        Sur] >- multiplicities d) -> applyM d [Str ("each "++(unCap.name.source) d)]
                                                                         [Str ("at most one "++(unCap.name.target) d)]
                                                              ++[Str (", whereas each "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d)]
                                                              ++[Str "."]
              | null ([    Tot,Inj    ] >- multiplicities d) -> applyM d [Str ("each "++(unCap.name.source) d)]
                                                                         [Str ("at least one "++(unCap.name.target) d)]
                                                              ++[Str (", whereas each "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d)]
                                                              ++[Str "."]
              | null ([    Tot,    Sur] >- multiplicities d) -> applyM d [Str ("each "++(unCap.name.source) d)]
                                                                         [Str ("at least one "++(unCap.name.target) d)]
                                                              ++[Str (" and each "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d)]
                                                              ++[Str "."]
              | null ([        Inj,Sur] >- multiplicities d) -> [Str ("There is exactly one "++(unCap.name.source) d++" (")]
                                                              ++[Math InlineMath "a"]
                                                              ++[Str (") for each "++(unCap.name.target) d++" (")]
                                                              ++[Math InlineMath "b"]
                                                              ++[Str "), for which: "]
                                                              ++applyM d [Math InlineMath "b"] [Math InlineMath "a"]
                                                              ++[Str "."]
              | null ([            Sur] >- multiplicities d) -> [Str ("There is at least one "++(unCap.name.source) d++" (")]
                                                              ++[Math InlineMath "a"]
                                                              ++[Str (") for each "++(unCap.name.target) d++" (")]
                                                              ++[Math InlineMath "b"]
                                                              ++[Str "), for which: "]
                                                              ++applyM d [Math InlineMath "b"] [Math InlineMath "a"]
                                                              ++[Str "."]
              | null ([        Inj    ] >- multiplicities d) -> [Str ("There is at most one "++(unCap.name.source) d++" (")]
                                                              ++[Math InlineMath "a"]
                                                              ++[Str (") for each "++(unCap.name.target) d++" (")]
                                                              ++[Math InlineMath "b"]
                                                              ++[Str "), for which: "]
                                                              ++applyM d [Math InlineMath "b"] [Math InlineMath "a"]
                                                              ++[Str "."]
              | null ([    Tot        ] >- multiplicities d) -> applyM d [Str ("each "++(unCap.name.source) d)]
                                                                         [Str ("at least one "++(unCap.name.target) d)]
                                                              ++[Str "."]
              | null ([Uni            ] >- multiplicities d) -> applyM d [Str ("each "++(unCap.name.source) d)]
                                                                         [Str ("zero or one "++(unCap.name.target) d)]
                                                              ++[Str "."]
              | otherwise                                    -> [Str "The sentence: "]
                                                              ++[Quoted DoubleQuote 
                                                                  (applyM d [Math InlineMath ((var [].source) d)]
                                                                            [Math InlineMath ((var [source d].target) d)])
                                                                ]
                                                              ++[Str (" is meaningful (i.e. it is either true or false) for any "++(unCap.name.source) d++" ")]
                                                              ++[(Math InlineMath . var [] . source) d]
                                                              ++[Str (" and "++(unCap.name.target) d++" ")]
                                                              ++[(Math InlineMath . var [source d] . target) d]
                                                              ++[Str "."]
      applyM :: Declaration -> [Inline] -> [Inline] -> [Inline]
      applyM decl a b =
               case decl of
                 Sgn{} | null (prL++prM++prR) 
                            -> a++[Space,Str "corresponds",Space,Str "to",Space]++b++[Space,Str "in",Space,Str "relation",Space,Str(decnm decl)]
                       | null prL
                            -> a++[Space,Str prM,Space]++b++[Space,Str prR]
                       | otherwise 
                            -> [Str (upCap prL),Space]++a++[Space,Str prM,Space]++b++if null prR then [] else [Space,Str prR]
                              where prL = decprL decl
                                    prM = decprM decl
                                    prR = decprR decl
                 Isn{}     -> a++[Space,Str "equals",Space]++b
                 Iscompl{} -> a++[Space,Str "differs",Space,Str "from",Space]++b
                 Vs{}      -> [Str (show True)]
            
      var :: Identified a => [a] -> a -> String     -- TODO Vervangen door mkvar, uit predLogic.hs
      var seen c = low c ++ ['\'' | c'<-seen, low c == low c']
                      where low idt= if null (name idt) then "x" else [(toLower.head.name) idt]

instance Explainable Rule where
  meaning l rule
   = head (expls++map explCont (autoMeaning l rule))
     where
        expls = [econt | Means l' econt<-rrxpl rule, l==l']
  explForObj x (ExplRule x') = x == x'
  explForObj _ _ = False
  explanations _ = []
  autoMeaning l r
   = [Expl { explObj   = ExplRule r                                       -- The object that is explained.
           , explPos   = origin r
           , explLang  = l                                   -- The language of the explaination
           , explRefId = ""                                               -- The reference of the explaination
           , explCont  = [Plain [RawInline "latex" (showPredLogic l r++".")]] -- The actual explanation.
           } ] 

--instance Explainable KeyDef where
--  explForObj x (ExplKeyDef x') = x == x'
--  explForObj _ _ = False
--
instance Explainable Pattern where
  meaning _ pat = fatal 324 ("Patterns have no intrinsic meaning, (used with pattern '"++name pat++"')")
  explForObj x (ExplPattern str) = name x == str
  explForObj _ _ = False
  explanations = ptxps

instance Explainable Process where
  meaning _ prc = fatal 329 ("Processes have no intrinsic meaning, (used with process '"++name prc++"')")
  explForObj x (ExplProcess str) = name x == str
  explForObj _ _ = False
  explanations = prcXps

instance Explainable Interface where
  meaning _ obj = fatal 342 ("Interfaces have no intrinsic meaning, (used with interface '"++name obj++"')")
  explForObj x (ExplInterface str) = name x == str
  explForObj _ _ = False
  explanations _ = []

--instance Explainable Activity where
--  explForObj act xpl = explForObj (actRule act) xpl
--
--instance Explainable P_Context where
--  explForObj x (ExplContext str) = name x == str
--  explForObj _ _ = False
--
instance Explainable Fspc where
  meaning _ fSpec = fatal 329 ("No Fspc has an intrinsic meaning, (used with Fspc '"++name fSpec++"')")
  explForObj x (ExplFspc str) = name x == str
  explForObj _ _ = False
  explanations fSpec
    = fSexpls fSpec ++
      (concatMap explanations . vpatterns)  fSpec ++
      (concatMap explanations . vprocesses) fSpec ++
      (concatMap explanations . interfaceS) fSpec

instance Explainable FProcess where
  meaning l fp = meaning l (proc fp)
  explForObj fp = explForObj (proc fp)
  explanations fp = explanations (proc fp)
  
--data ExplainOutputFormat = PlainText 
--format  :: ExplainOutputFormat -> [Explanation] -> String
--format fmtType expls
--   = intercalate "\n" (map (formatOne fmtType) expls)
--   where formatOne ::  ExplainOutputFormat -> Explanation -> String
--         formatOne PlainText expl = explainContent2String (explCont expl)
--
