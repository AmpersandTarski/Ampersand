--TODO -> Maybe this module is useful at more places than just func spec rendering.
--        In that case it's not a Rendering module and it needs to be replaced
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.FSpec.Motivations 
   ( Motivated (purposesDefinedIn),
     HasMeaning(meaning,meanings))
where
import Ampersand.ADL1
import Ampersand.FSpec.FSpec(FSpec(..)) 
import Ampersand.Basics

-- The general idea is that an Ampersand relation such as:
--     PURPOSE RELATION r[A*B] IN ENGLISH
--     {+This text explains why r[A*B] exists+}
-- produces the exact right text in the functional design document.

-- The class Motivated exists so that we can write the Haskell expression 'purposesDefinedIn fSpec l x'
-- anywhere we like for every type of x that could possibly be motivated in an Purpose.
-- 'purposesDefinedIn fSpec l x' produces all purposes related to x from the context (fSpec)
-- that are available in the language specified in 'l'.
-- The other functions in this class are solely meant to be used in the definition of purpose.
-- They are defined once for each instance of Explainable, not be used in other code.

class  Named a => Motivated a where
  isForObject :: a -> ExplObj -> Bool    -- ^ Given an Explainable object and an ExplObj, return TRUE if they concern the identical object.
  purposesDefinedIn :: FSpec -> Lang -> a -> [Purpose]  -- ^ The purposes defined for a specific a, given Language.
  purposesDefinedIn fSpec l x
   = [e | e<-fSexpls fSpec
        , amLang (explMarkup e) == l  -- filter by language
        , isForObject x (explObj e)   -- informally: "if x and e are the same"
     ]
instance Motivated A_Concept where
--  meaning _ c = fatal ("Concepts have no intrinsic meaning, (used with concept '"<>name c<>"')")
  isForObject c1 (ExplConcept c2) = c1 == c2
  isForObject _ _ = False

instance Motivated Relation where
  isForObject d1 (ExplRelation d2) = d1 == d2
  isForObject _ _ = False

instance Motivated Rule where
  isForObject x (ExplRule str) = name x == str
  isForObject _ _ = False

instance Motivated Pattern where
  isForObject x (ExplPattern str) = name x == str
  isForObject _ _ = False

instance Motivated Interface where
  isForObject x (ExplInterface str) = name x == str
  isForObject _ _ = False



class Named a => HasMeaning a where
  meaning :: Lang -> a -> Maybe Meaning
  meaning l x = 
     case filter (\(Meaning m) -> l == amLang m) (meanings x) of
       []   -> Nothing
       [m]  -> Just m
       _    -> fatal ("In the "<>tshow l<>" language, too many meanings given for "<>name x <>".")             
  meanings :: a -> [Meaning]
  {-# MINIMAL meanings #-}

instance HasMeaning Rule where
  meanings = rrmean
--  meaning l rule
--   = head (expls<>map explCont (autoMeaning l rule))
--     where
--        expls = [econt | Means l' econt<-rrxpl rule, l'==Just l || l'==Nothing]
--  autoMeaning lang r
--   = [Expl { explObj   = ExplRule (name r)
--           , explPos   = origin r
--           , explLang  = Just lang
--           , explRefIds = []
--           , explCont  = [Plain [RawInline (Text.Pandoc.Builder.Format "latex") (showPredLogic lang r<>".")]]
--           } ]

instance HasMeaning Relation where
  meanings = decMean
--  meaning l decl = if null (decMean decl)
--                   then concat [explCont expl | expl<-autoMeaning l decl, Just l == explLang expl || Nothing == explLang expl]
--                   else decMean decl
--  autoMeaning lang d
--   = [Expl { explPos   = decfpos d
--           , explObj   = ExplRelation d
--           , explLang  = Just lang
--           , explRefIds = []
--           , explCont  = [Para langInlines]
--           } ]
--     where
--      langInlines =
--       case lang of
--         English
--              | null ([Sym,Asy]         Set.\\ properties d) -> [Emph [Str (name d)]]
--                                                              <>[Str " is a property of a "]
--                                                              <>[Str ((unCap.name.source) d)]
--                                                              <>[Str "."]
--              | null ([Sym,Rfx,Trn]     Set.\\ properties d) -> [Emph [Str (name d)]]
--                                                              <>[Str " is an equivalence relation on "]
--                                                              <>[Str ((unCap.plural English .name.source) d)]
--                                                              <>[Str "."]
--              | null ([Asy,Trn]         Set.\\ properties d) -> [Emph [Str (name d)]]
--                                                              <>[Str " is an ordering relation on "]
--                                                              <>[Str ((unCap.plural English .name.source) d)]
--                                                              <>[Str "."]
--              | null ([Uni,Tot,Inj,Sur] Set.\\ properties d) -> applyM d [Str ("each "<>(unCap.name.source) d)]
--                                                                         [Str ("exactly one "<>(unCap.name.target) d)]
--                                                              <>[Str " and vice versa."]
--              | null ([Uni,Tot,Inj    ] Set.\\ properties d) -> applyM d [Str ("each "<>(unCap.name.source) d)]
--                                                                         [Str ("exactly one "<>(unCap.name.target) d)]
--                                                              <>[Str ", but not for each "]
--                                                              <>[Str ((unCap.name.target) d<>" there must be a "<>(unCap.name.source) d)]
--                                                              <>[Str "."]
--              | null ([Uni,Tot,    Sur] Set.\\ properties d) -> applyM d [Str ("each "<>(unCap.name.source) d)]
--                                                                         [Str ("exactly one "<>(unCap.name.target) d)]
--                                                              <>[Str ", but each "]
--                                                              <>[Str ((unCap.name.target) d<>" is related to one or more "<>(unCap.plural English .name.source) d)]
--                                                              <>[Str "."]
--              | null ([Uni,    Inj,Sur] Set.\\ properties d) -> [Str ("There is exactly one "<>(unCap.name.source) d<>" (")]
--                                                              <>[Math InlineMath "a"]
--                                                              <>[Str (") for each "<>(unCap.name.target) d<>" (")]
--                                                              <>[Math InlineMath "b"]
--                                                              <>[Str "), for which: "]
--                                                              <>applyM d [Math InlineMath "b"] [Math InlineMath "a"]
--                                                              <>[Str (", but not for each "<>(unCap.name.source) d<>" there must be a "<>(unCap.name.target) d<>".")]
--              | null ([    Tot,Inj,Sur] Set.\\ properties d) -> [Str ("There is exactly one "<>(unCap.name.source) d<>" (")]
--                                                              <>[Math InlineMath "a"]
--                                                              <>[Str (") for each "<>(unCap.name.target) d<>" (")]
--                                                              <>[Math InlineMath "b"]
--                                                              <>[Str "), for which: "]
--                                                              <>applyM d [Math InlineMath "b"] [Math InlineMath "a"]
--                                                              <>[Str (", but each "<>(unCap.name.source) d<>" is related to one or more "<>(unCap.plural English .name.target) d)]
--                                                              <>[Str "."]
--              | null ([Uni,Tot        ] Set.\\ properties d) -> applyM d [Str ("each "<>(unCap.name.source) d)]
--                                                                         [Str ("exactly one "<>(unCap.name.target) d)]
--                                                              <>[Str "."]
--              | null ([Uni,    Inj    ] Set.\\ properties d) -> applyM d [Str ("each "<>(unCap.name.source) d)]
--                                                                         [Str ("at most one "<>(unCap.name.target) d)]
--                                                              <>[Str (" and each "<>(unCap.name.target) d<>" is related to at most one "<>(unCap.name.source) d)]
--                                                              <>[Str "."]
--              | null ([Uni,        Sur] Set.\\ properties d) -> applyM d [Str ("each "<>(unCap.name.source) d)]
--                                                                         [Str ("at most one "<>(unCap.name.target) d)]
--                                                              <>[Str (", whereas each "<>(unCap.name.target) d<>" is related to at least one "<>(unCap.name.source) d)]
--                                                              <>[Str "."]
--              | null ([    Tot,Inj    ] Set.\\ properties d) -> applyM d [Str ("each "<>(unCap.name.source) d)]
--                                                                         [Str ("at least one "<>(unCap.name.target) d)]
--                                                              <>[Str (", whereas each "<>(unCap.name.target) d<>" is related to at most one "<>(unCap.name.source) d)]
--                                                              <>[Str "."]
--              | null ([    Tot,    Sur] Set.\\ properties d) -> applyM d [Str ("each "<>(unCap.name.source) d)]
--                                                                         [Str ("at least one "<>(unCap.name.target) d)]
--                                                              <>[Str (" and each "<>(unCap.name.target) d<>" is related to at least one "<>(unCap.name.source) d)]
--                                                              <>[Str "."]
--              | null ([        Inj,Sur] Set.\\ properties d) -> [Str ("There is exactly one "<>(unCap.name.source) d<>" (")]
--                                                              <>[Math InlineMath "a"]
--                                                              <>[Str (") for each "<>(unCap.name.target) d<>" (")]
--                                                              <>[Math InlineMath "b"]
--                                                              <>[Str "), for which: "]
--                                                              <>applyM d [Math InlineMath "b"] [Math InlineMath "a"]
--                                                              <>[Str "."]
--              | null ([            Sur] Set.\\ properties d) -> [Str ("There is at least one "<>(unCap.name.source) d<>" (")]
--                                                              <>[Math InlineMath "a"]
--                                                              <>[Str (") for each "<>(unCap.name.target) d<>" (")]
--                                                              <>[Math InlineMath "b"]
--                                                              <>[Str "), for which: "]
--                                                              <>applyM d [Math InlineMath "b"] [Math InlineMath "a"]
--                                                              <>[Str "."]
--              | null ([        Inj    ] Set.\\ properties d) -> [Str ("There is at most one "<>(unCap.name.source) d<>" (")]
--                                                              <>[Math InlineMath "a"]
--                                                              <>[Str (") for each "<>(unCap.name.target) d<>" (")]
--                                                              <>[Math InlineMath "b"]
--                                                              <>[Str "), for which: "]
--                                                              <>applyM d [Math InlineMath "b"] [Math InlineMath "a"]
--                                                              <>[Str "."]
--              | null ([    Tot        ] Set.\\ properties d) -> applyM d [Str ("each "<>(unCap.name.source) d)]
--                                                                         [Str ("at least one "<>(unCap.name.target) d)]
--                                                              <>[Str "."]
--              | null ([Uni            ] Set.\\ properties d) -> applyM d [Str ("each "<>(unCap.name.source) d)]
--                                                                         [Str ("zero or one "<>(unCap.name.target) d)]
--                                                              <>[Str "."]
--              | otherwise                                    -> [Str "The sentence: "]
--                                                              <>[Quoted DoubleQuote
--                                                                  (applyM d [Math InlineMath ((var [].source) d)]
--                                                                            [Math InlineMath ((var [source d].target) d)])
--                                                                ]
--                                                              <>[Str (" is meaningful (i.e. it is either true or false) for any "<>(unCap.name.source) d<>" ")]
--                                                              <>[(Math InlineMath . var [] . source) d]
--                                                              <>[Str (" and "<>(unCap.name.target) d<>" ")]
--                                                              <>[(Math InlineMath . var [source d] . target) d]
--                                                              <>[Str "."]
--         Dutch
--              | null ([Sym,Asy]         Set.\\ properties d) -> [Emph [Str (name d)]]
--                                                              <>[Str " is een eigenschap van een "]
--                                                              <>[Str ((unCap.name.source) d)]
--                                                              <>[Str "."]
--              | null ([Sym,Rfx,Trn]     Set.\\ properties d) ->[Emph [Str (name d)]]
--                                                              <>[Str " is een equivalentierelatie tussen "]
--                                                              <>[Str ((unCap.plural Dutch .name.source) d)]
--                                                              <>[Str "."]
--              | null ([Asy,Trn]         Set.\\ properties d) ->[Emph [Str (name d)]]
--                                                              <>[Str " is een ordeningsrelatie tussen "]
--                                                              <>[Str ((unCap.plural Dutch .name.source) d)]
--                                                              <>[Str "."]
--              | null ([Uni,Tot,Inj,Sur] Set.\\ properties d) -> applyM d [Str ("elke "<>(unCap.name.source) d)]
--                                                                              [Str ("precies één "<>(unCap.name.target) d)]
--                                                              <>[Str " en vice versa."]
--              | null ([Uni,Tot,Inj]     Set.\\ properties d) -> applyM d [Str ("elke "<>(unCap.name.source) d)]
--                                                                         [Str ("precies één "<>(unCap.name.target) d)]
--                                                              <>[Str ", maar niet voor elke "]
--                                                              <>[Str ((unCap.name.target) d)]
--                                                              <>[Str " hoeft er een "]
--                                                              <>[Str ((unCap.name.source) d)]
--                                                              <>[Str " te zijn."]
--              | null ([Uni,Tot,    Sur] Set.\\ properties d) -> applyM d [Str ("elke "<>(unCap.name.source) d)]
--                                                                         [Str ("precies één "<>(unCap.name.target) d)]
--                                                              <>[Str ", maar elke "]
--                                                              <>[Str ((unCap.name.target) d)]
--                                                              <>[Str (" is gerelateerd aan één of meer ")]
--                                                              <>[Str ((unCap.plural Dutch .name.source) d)]
--                                                              <>[Str "."]
--              | null ([Uni,    Inj,Sur] Set.\\ properties d) -> [Str ("Er is precies één "<>(unCap.name.source) d<>" (")]
--                                                              <>[Math InlineMath "a"]
--                                                              <>[Str (") voor elke "<>(unCap.name.target) d<>" (")]
--                                                              <>[Math InlineMath "b"]
--                                                              <>[Str "), waarvoor geldt: "]
--                                                              <>applyM d [Math InlineMath "b"] [Math InlineMath "a"]
--                                                              <>[Str (", maar niet voor elke "<>(unCap.name.source) d<>" hoeft er een "<>(unCap.name.target) d<>" te zijn.")]
--              | null ([    Tot,Inj,Sur] Set.\\ properties d) -> [Str ("Er is precies één "<>(unCap.name.source) d<>" (")]
--                                                              <>[Math InlineMath "a"]
--                                                              <>[Str (") voor elke "<>(unCap.name.target) d<>" (")]
--                                                              <>[Math InlineMath "b"]
--                                                              <>[Str "), waarvoor geldt: "]
--                                                              <>applyM d [Math InlineMath "b"] [Math InlineMath "a"]
--                                                              <>[Str (", maar elke "<>(unCap.name.source) d<>" mag gerelateerd zijn aan meerdere "<>(unCap.plural Dutch .name.target) d<>".")]
--              | null ([Uni,Tot        ] Set.\\ properties d) -> applyM d [Str ("elke "<>(unCap.name.source) d)]
--                                                                         [Str ("precies één "<>(unCap.name.target) d)]
--                                                              <>[Str "."]
--              | null ([Uni,    Inj    ] Set.\\ properties d) -> applyM d [Str ("elke "<>(unCap.name.source) d)]
--                                                                         [Str ("ten hoogste één "<>(unCap.name.target) d)]
--                                                              <>[Str " en elke "]
--                                                              <>[Str ((unCap.name.target) d)]
--                                                              <>[Str (" is gerelateerd aan ten hoogste één ")]
--                                                              <>[Str ((unCap.name.source) d<>".")]
--                                                              <>[Str "."]
--              | null ([Uni,        Sur] Set.\\ properties d) -> applyM d [Str ("elke "<>(unCap.name.source) d)]
--                                                                         [Str ("ten hoogste één "<>(unCap.name.target) d)]
--                                                              <>[Str ", terwijl elke "]
--                                                              <>[Str ((unCap.name.target) d)]
--                                                              <>[Str (" is gerelateerd aan tenminste één ")]
--                                                              <>[Str ((unCap.name.source) d)]
--                                                              <>[Str "."]
--              | null ([    Tot,Inj    ] Set.\\ properties d) -> applyM d [Str ("elke "<>(unCap.name.source) d)]
--                                                                         [Str ("tenminste één "<>(unCap.name.target) d)]
--                                                              <>[Str ", terwijl elke "]
--                                                              <>[Str ((unCap.name.target) d)]
--                                                              <>[Str (" is gerelateerd aan ten hoogste één ")]
--                                                              <>[Str ((unCap.name.source) d)]
--                                                              <>[Str "."]
--              | null ([    Tot,    Sur] Set.\\ properties d) -> applyM d [Str ("elke "<>(unCap.name.source) d)]
--                                                                         [Str ("tenminste één "<>(unCap.name.target) d)]
--                                                              <>[Str (" en elke "<>(unCap.name.target) d<>" is gerelateerd aan tenminste één "<>(unCap.name.source) d<>".")]
--              | null ([        Inj,Sur] Set.\\ properties d) -> [Str ("Er is precies één "<>(unCap.name.source) d<>" (")]
--                                                              <>[Math InlineMath "a"]
--                                                              <>[Str (") voor elke "<>(unCap.name.target) d<>" (")]
--                                                              <>[Math InlineMath "b"]
--                                                              <>[Str "), waarvoor geldt: "]
--                                                              <>applyM d [Math InlineMath "b"] [Math InlineMath "a"]
--                                                              <>[Str "."]
--              | null ([            Sur] Set.\\ properties d) -> [Str ("Er is tenminste één "<>(unCap.name.source) d<>" (")]
--                                                              <>[Math InlineMath "a"]
--                                                              <>[Str (") voor elke "<>(unCap.name.target) d<>" (")]
--                                                              <>[Math InlineMath "b"]
--                                                              <>[Str "), waarvoor geldt: "]
--                                                              <>applyM d [Math InlineMath "b"] [Math InlineMath "a"]
--                                                              <>[Str "."]
--              | null ([        Inj    ] Set.\\ properties d) -> [Str ("Er is hooguit één "<>(unCap.name.source) d<>" (")]
--                                                              <>[Math InlineMath "a"]
--                                                              <>[Str (") voor elke "<>(unCap.name.target) d<>" (")]
--                                                              <>[Math InlineMath "b"]
--                                                              <>[Str "), waarvoor geldt: "]
--                                                              <>applyM d [Math InlineMath "b"] [Math InlineMath "a"]
--                                                              <>[Str "."]
--              | null ([    Tot        ] Set.\\ properties d) -> applyM d [Str ("elke "<>(unCap.name.source) d)]
--                                                                         [Str ("tenminste één "<>(unCap.name.target) d)]
--                                                              <>[Str "."]
--              | null ([Uni            ] Set.\\ properties d) -> applyM d [Str ("elke "<>(unCap.name.source) d)]
--                                                                         [Str ("nul of één "<>(unCap.name.target) d)]
--                                                              <>[Str "."]
--              | otherwise                                    -> [Str "De zin: "]
--                                                              <>[Quoted DoubleQuote
--                                                                  (applyM d [(Math InlineMath . var [] . source) d]
--                                                                            [(Math InlineMath . var [source d] . target) d])
--                                                                ]
--                                                              <>[Str (" heeft betekenis (dus: is waar of niet waar) voor een "<>(unCap.name.source) d<>" ")]
--                                                              <>[(Math InlineMath . var [].source) d]
--                                                              <>[Str (" en een "<>(unCap.name.target) d<>" ")]
--                                                              <>[(Math InlineMath . var [source d].target) d]
--                                                              <>[Str "."]
--
--      applyM :: Relation -> [Inline] -> [Inline] -> [Inline]
--      applyM decl a b
--              | null (prL<>prM<>prR)
--                   = a<>[Space,Str "corresponds",Space,Str "to",Space]<>b<>[Space,Str "in",Space,Str "relation",Space,Str(name decl)]
--              | null prL
--                   = a<>[Space,Str prM,Space]<>b<>[Space,Str prR]
--              | otherwise
--                   = [Str (upCap prL),Space]<>a<>[Space,Str prM,Space]<>b<>if null prR then [] else [Space,Str prR]
--          where prL = decprL decl
--                prM = decprM decl
--                prR = decprR decl
--
--      var :: Named a => [a] -> a -> String     -- TODO Vervangen door mkvar, uit predLogic.hs
--      var seen c = low c <> ['\'' | c'<-seen, low c == low c']
--                      where low idt= if null (name idt) then "x" else [(toLower.head.name) idt]

instance Motivated FSpec where
  isForObject x (ExplContext str) = name x == str
  isForObject _ _ = False

