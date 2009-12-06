{-# OPTIONS_GHC -Wall #-}
module Classes.ViewPoint (ViewPoint(..)) 
where
   import Adl.Context                 (Context(..))
   import Adl.Pattern                 (Pattern(..),union)
   import Adl.Rule                    (Rule(..),RuleType(..))
   import Adl.ObjectDef               (ObjectDef(..))
   import Adl.MorphismAndDeclaration  (Morphism(..),mIs)
   import Adl.Concept                 (Concept(..),Association(..),Morphic(..))
--   import Adl.Prop                    (Prop(..))
   import Adl.Expression              (Expression(..))
   import Adl.FilePos                 (Numbered(..),FilePos(..))
   import Adl.Gen                     (Gen(G))
   import Classes.Morphical           (Morphical(..))
--   import Classification              (Classification,preCl)
   import Collection                  (Collection(..))
   import CommonClasses               (Identified(..))
   import Typology                    (Inheritance(..))
   import Auxiliaries                 (enumerate) 

   class Morphical a => ViewPoint a where
     objectdef      :: a -> ObjectDef        -- The objectdef that characterizes this viewpoint
     rules          :: a -> [Rule]      -- all rules in the language that hold within this viewpoint
     changeable     :: a -> Morphism -> Bool
     signals        :: a -> [Rule]      -- all SIGNAL rules in the language.
     patterns       :: a -> [Pattern]   -- all patterns that are used in this viewpoint
     patterns _      = []
     isa            :: a -> Inheritance Concept
     isa _           = empty

   instance ViewPoint a => ViewPoint [a] where
    objectdef xs     = Obj { objnm   = ""         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                           , objpos  = Nowhere    -- ^ position of this definition in the text of the ADL source file (filename, line number and column number)
                           , objctx  = Tm (mIs S) -- ^ this expression describes the instances of this object, related to their context. 
                           , objats  = []         -- ^ the attributes, which are object definitions themselves.
                           , objstrs = []         -- ^ directives that specify the interface.
                           }
    rules xs         = (concat. map rules) xs
    changeable xs m  = or [changeable x m | x<-xs]
    signals xs       = (concat. map signals) xs
    patterns         = rd' name.concat.map patterns
    isa              = foldr uni empty.map isa

   instance ViewPoint Context where
    --Interpretation of context as a language means to describe the classification tree,
    --the set of declarations and the rules that apply in that context. Inheritance of
    --properties is achieved as a result.
    objectdef context = Obj { objnm   = name context
                            , objpos  = Nowhere
                            , objctx  = Tm (mIs S)
                            , objats  = map objectdef (ctxpats context)
                            , objstrs = []
                            }
    rules     context = rd ([r| r<-ctxrs context, not (isSignal r)] ++ (concat.map ptrls.ctxpats) context )
    signals   context = rd ([r| r<-ctxrs context,      isSignal r ] ++ signals (ctxpats context))
    patterns  context = ctxpats context
    isa       context = ctxisa  context

   instance ViewPoint Pattern where
    objectdef pat = Obj { objnm   = name pat
                        , objpos  = Nowhere
                        , objctx  = Tm (mIs S)
                        , objats  = []
                        , objstrs = []
                        }
    rules pat     = [r|r<-ptrls pat, not (isSignal r)]
    signals pat   = [r|r<-ptrls pat,      isSignal r ]
    patterns pat  = [pat]
    isa pat       = Isa ts (concs pat>-[e'| G _ g s<-ptgns pat,e'<-[g,s]])
                    where ts = clear [(g,s)| G _ g s<-ptgns pat]


   instance ViewPoint Rule where
    objectdef rule = Obj { objnm   = name rule
                         , objpos  = pos rule
                         , objctx  = Tm (mIs S)
                         , objats  = []
                         , objstrs = []
                         }
    rules r    = case r of 
                 Ru{} -> [r| not (isSignal r)]
                 Sg{} -> []
                 Fr{} -> []
                       
    signals r = case r of
                  Ru{} -> []
                  Sg{} -> [r| isSignal r]
                  Fr{} -> [] 

    patterns r = [Pat{ ptnm  = ""
                     , ptrls = [r]
                     , ptgns = []
                     , ptdcs = []
                     , ptcds = []
                     , ptkds = []}]

    isa r = empty

   clear :: [(Concept,Concept)] -> [(Concept,Concept)]
   clear abs' = rd [(a,b)| (a,b)<-abs', a/=b]

