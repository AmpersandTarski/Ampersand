{-# OPTIONS_GHC -Wall #-}
module Classes.Language   (Language( declaredRules
                                   , multRules
                                   , rules
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
     declaredRules  :: a -> [Rule] -- all rules in the language that are specified as a rule in the ADL-model, including the GLUE rules, but excluding the multiplicity rules (multRules).
     declaredRules _ = []
     multRules      :: a -> [Rule] -- all rules in the language that are specified as declaration properties.
     multRules       = multRules.declarations
     rules          :: a -> [Rule] -- all rules in the language that hold within the language
     rules x         = declaredRules x++multRules x++specs x
     signals        :: a -> [Rule] -- all SIGNAL rules in the language.
     signals _       = []
     specs          :: a -> [Rule] -- all GLUE rules in the language.
     specs _         = []
     patterns       :: a -> [Pattern]
     patterns _      = []
     objectdefs     :: a -> [ObjectDef]
     objectdefs _    = []
     isa            :: a -> Inheritance Concept
     isa _           = empty

   instance Language a => Language [a] where
    declaredRules xs = (concat. map declaredRules) xs
    multRules xs     = (concat. map multRules) xs
    signals xs       = (concat. map signals) xs
    specs xs         = (rd. concat. map specs) xs
    patterns         = {- rd' name. -} concat.map patterns
    objectdefs       = {- rd' name. -} concat.map objectdefs
    isa              = foldr uni empty.map isa

   instance Language a => Language (Classification a) where
    declaredRules cl = declaredRules (preCl cl)
    multRules cl     = multRules (preCl cl)
    signals cl       = signals (preCl cl)
    specs cl         = specs (preCl cl)
    patterns cl      = patterns (preCl cl)
    objectdefs cl    = objectdefs (preCl cl)
    isa              = foldr uni empty.map isa.preCl

   instance Language Context where
    --Interpretation of context as a language means to describe the classification tree,
    --the set of declarations and the rules that apply in that context. Inheritance of
    --properties is achieved as a result.
    declaredRules context = declaredRules (foldr union (Pat "" [] [] [] [] []) (ctxpats context))
    multRules     context = multRules     (foldr union (Pat "" [] [] [] [] []) (ctxpats context))
    rules         context = [r| r<-ctxrs context, not (isSignal r)]
   -- rules         (Ctx nm on i world pats rs ds cs ks os pops) = [r| r<-rs, not (isSignal r)]
    signals       context = signals       (foldr union (Pat "" [] [] [] [] []) (ctxpats context))
    specs         context = specs         (foldr union (Pat "" [] [] [] [] []) (ctxpats context))
    patterns      context = ctxpats context
    objectdefs    context = ctxos   context
    isa           context = ctxisa  context

   instance Language Declaration where
    multRules d
     = [h p| p<-multiplicities d, p `elem` [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
           , if source d==target d || p `elem` [Uni,Tot,Inj,Sur] then True else
              error ("!Fatal (module CC_aux): Property "++show p++" requires equal source and target domains (you specified "++name (source d)++" and "++name (target d)++").") ]
      where h Sym = Ru Equivalence (F [Tm r]) (pos d) (F [Tm r'])        [] (name d++"["++name (source d)++"*"++name (source d)++"] is symmetric.")     sgn Nothing (nr d) ""
            h Asy = Ru Implication (Fi [F [Tm r], F [Tm r']]) (pos d) id' [] (name d++"["++name (source d)++"*"++name (source d)++"] is antisymmetric.") sgn Nothing (nr d) ""
            h Trn = Ru Implication (F [Tm r, Tm r]) (pos d) (F [Tm r])   [] (name d++"["++name (source d)++"*"++name (source d)++"] is transitive.")    sgn Nothing (nr d) ""
            h Rfx = Ru Implication id' (pos d) (F [Tm r])                 [] (name d++"["++name (source d)++"*"++name (source d)++"] is reflexive.")     sgn Nothing (nr d) ""
            h Uni = Ru Implication (F [Tm r',Tm r]) (pos d) id''          [] (name d++"["++name (source d)++"*"++name (target d)++"] is univalent")      sgn Nothing (nr d) ""
            h Sur = Ru Implication id'' (pos d) (F [Tm r',Tm r])          [] (name d++"["++name (source d)++"*"++name (target d)++"] is surjective")     sgn Nothing (nr d) ""
            h Inj = Ru Implication (F [Tm r,Tm r']) (pos d) id'           [] (name d++"["++name (source d)++"*"++name (target d)++"] is injective")      sgn Nothing (nr d) ""
            h Tot = Ru Implication id' (pos d) (F [Tm r,Tm r'])           [] (name d++"["++name (source d)++"*"++name (target d)++"] is total")          sgn Nothing (nr d) ""
            h Aut = error("!Fatal (module Language): multRules not defined for property 'Aut'")
            sgn   = (source d,source d)
            r     = Mph (name d)                (pos d) [] (source d,target d) True d
            r'    = flp (r ) 
 --           r'' t = Mph (t++"["++(name d)++"]") (pos d) [] (source d,target d) True d
            id'    = F [Tm (I [source d] (source d) (source d) True)]
            id''    = F [Tm (I [target d] (target d) (target d) True)]
 

   instance Language Pattern where
    declaredRules pat = [r|r@(Ru{})<-ptrls pat]
    rules _           = []
    signals pat       = [r|r@(Sg{})<-ptrls pat]
    specs pat         = [r|r@(Gc{})<-ptrls pat]
    patterns pat      = [pat]
    isa pat           = Isa ts (singles>-[e'| G _ g s<-ptgns pat,e'<-[g,s]])
                        where Isa tuples singles = isa (ptdcs pat)
                              ts = clear (tuples++[(g,s)| G _ g s<-ptgns pat])


   instance Language Rule where
      declaredRules r = case r of 
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
                         
--    Was eerst: 
--    declaredRules r@(Gc pos m expr cpu sgn nr pn)
--     = [Ru Equivalence (F [Tm m]) pos expr cpu (name m++" is implemented using "++enumerate (map name (mors expr))) sgn nr pn]
--    declaredRules   r@(Ru _ _ _ _ _ _ _ _ _) = [r]
--    declaredRules   r                        = []
      rules _     = []

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

