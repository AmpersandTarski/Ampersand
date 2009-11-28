{-# OPTIONS_GHC -Wall #-}
module Classes.Language   (Language( rules
                                   , signals
                                   , specs
                                   , patterns
                                   , objectdefs
                                   , isa
                           )       ) 
where
   import Adl.Context                 (Context(..))
   import Adl.Pattern                 (Pattern(..),union)
   import Adl.Rule                    (Rule(..),RuleType(..))
   import Adl.ObjectDef               (ObjectDef)
   import Adl.MorphismAndDeclaration  (Morphism(..),Declaration)
   import Adl.Concept                 (Concept(..),Association(..),Morphic(..))
   import Adl.Prop                    (Prop(..))
   import Adl.Expression              (Expression(..))
   import Adl.FilePos                 (Numbered(..))
   import Adl.Gen                     (Gen(G))
   import Classes.Morphical           (Morphical(..))
   import Classification              (Classification,preCl)
   import Collection                  (Collection(..))
   import CommonClasses               (Identified(..))
   import Typology                    (Inheritance(..))
   import Auxiliaries                 (enumerate) 

   class Morphical a => Language a where
     rules          :: a -> [Rule] -- all rules in the language that hold within the language
     signals        :: a -> [Rule] -- all SIGNAL rules in the language.
     specs          :: a -> [Rule] -- all GLUE rules in the language.
     specs _         = []
     patterns       :: a -> [Pattern]
     patterns _      = []
     objectdefs     :: a -> [ObjectDef]
     objectdefs _    = []
     isa            :: a -> Inheritance Concept
     isa _           = empty

   instance Language a => Language [a] where
    rules xs         = (concat. map rules) xs
    signals xs       = (concat. map signals) xs
    specs xs         = (rd. concat. map specs) xs
    patterns         = {- rd' name. -} concat.map patterns
    objectdefs       = {- rd' name. -} concat.map objectdefs
    isa              = foldr uni empty.map isa

   instance Language a => Language (Classification a) where
    rules cl         = rules (preCl cl)
    signals cl       = signals (preCl cl)
    specs cl         = specs (preCl cl)
    patterns cl      = patterns (preCl cl)
    objectdefs cl    = objectdefs (preCl cl)
    isa              = foldr uni empty.map isa.preCl

   instance Language Context where
    --Interpretation of context as a language means to describe the classification tree,
    --the set of declarations and the rules that apply in that context. Inheritance of
    --properties is achieved as a result.
    rules         context = [r| r<-ctxrs context, not (isSignal r)]
    signals       context = signals       (foldr union (Pat "" [] [] [] [] []) (ctxpats context))
    specs         context = specs         (foldr union (Pat "" [] [] [] [] []) (ctxpats context))
    patterns      context = ctxpats context
    objectdefs    context = ctxos   context
    isa           context = ctxisa  context

   instance Language Pattern where
    rules pat    = [r|r@(Ru{})<-ptrls pat]
    signals pat  = [r|r@(Sg{})<-ptrls pat]
    specs pat    = [r|r@(Gc{})<-ptrls pat]
    patterns pat = [pat]
    isa pat      = Isa ts (concs pat>-[e'| G _ g s<-ptgns pat,e'<-[g,s]])
                   where ts = clear [(g,s)| G _ g s<-ptgns pat]


   instance Language Rule where
      rules r    = case r of 
                   Ru{} -> [r]
                   Sg{} -> []
                   Gc{} -> [Ru {rrsrt = Equivalence
                               ,rrant = F [Tm (grspe r)]
                               ,rrfps = grfps r
                               ,rrcon = grgen r
                               ,r_cpu = r_cpu r
                               ,rrxpl = name r++" is implemented using "++enumerate (map name (mors (grgen r)))
                               ,rrtyp = grtyp r
                               ,runum = runum r
                               ,r_pat = r_pat r
                              }]
                   Fr{} -> []
                         
      signals r = case r of
                    Ru{} -> []
                    Sg{} -> [r]
                    Gc{} -> []
                    Fr{} -> [] 

      specs r = case r of
                    Ru{} -> []
                    Sg{} -> []
                    Gc{} -> [r]
                    Fr{} -> [] 
      patterns r = [Pat{ ptnm  = ""
                       , ptrls = [r]
                       , ptgns = []
                       , ptdcs = []
                       , ptcds = []
                       , ptkds = []}]
      isa r = case r of
                    Ru{} -> empty
                    Sg{} -> empty
                    Gc{} -> Isa tuples (concs (grgen r)>-[e'|(a,b)<-tuples,e'<-[a,b]])
                    Fr{} -> empty 
                  where tuples = clear [(source(grgen r),source(grspe r))
                                       ,(target(grgen r),target(grspe r))
                                       ]  

   clear :: [(Concept,Concept)] -> [(Concept,Concept)]
   clear abs' = rd [(a,b)| (a,b)<-abs', a/=b]

