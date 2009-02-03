
module Classes.Language where
   import Classes.Morphical
   import Classes.Morphic
   import Adl.Context
   import Adl.Pattern
   import Adl.Rule
   import Adl.ObjectDef
   import Adl.MorphismAndDeclaration
   import Adl.Concept
   import Adl.Prop
   import Adl.Expression
   import Adl.FilePos
   import Adl.Gen
   import Classification(Classification,preCl)
   import Collection
   import CommonClasses ( Identified(name))
   import Typology ( Inheritance(Isa))--, Typologic(typology), genEq)
   import Auxiliaries (enumerate) 

   class Morphical a => Language a where
     declaredRules  :: a -> [Rule] -- all rules in the language that are specified as a rule in the ADL-model, including the GLUE rules, but excluding the multiplicity rules (multRules).
     declaredRules x = []
     multRules      :: a -> [Rule] -- all rules in the language that are specified as declaration properties.
     multRules       = multRules.declarations
     rules          :: a -> [Rule] -- all rules in the language that hold within the language
     rules x         = declaredRules x++multRules x++specs x
     signals        :: a -> [Rule] -- all SIGNAL rules in the language.
     signals x       = []
     specs          :: a -> [Rule] -- all GLUE rules in the language.
     specs x         = []
     patterns       :: a -> [Pattern]
     patterns x      = []
     objectdefs     :: a -> [ObjectDef]
     objectdefs x    = []
     isa            :: a -> Inheritance Concept
     isa x           = empty

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
      where h Sym = Ru Equivalence (F [Tm r]) (pos d) (F [Tm r'])        [] (name d++"["++name (source d)++"*"++name (source d)++"] is symmetric.")     sgn (nr d) ""
            h Asy = Ru Implication (Fi [F [Tm r], F [Tm r']]) (pos d) id [] (name d++"["++name (source d)++"*"++name (source d)++"] is antisymmetric.") sgn (nr d) ""
            h Trn = Ru Implication (F [Tm r, Tm r]) (pos d) (F [Tm r])   [] (name d++"["++name (source d)++"*"++name (source d)++"] is transitive.")    sgn (nr d) ""
            h Rfx = Ru Implication id (pos d) (F [Tm r])                 [] (name d++"["++name (source d)++"*"++name (source d)++"] is reflexive.")     sgn (nr d) ""
            h Uni = Ru Implication (F [Tm r',Tm r]) (pos d) id'          [] (name d++"["++name (source d)++"*"++name (target d)++"] is univalent")      sgn (nr d) ""
            h Sur = Ru Implication id' (pos d) (F [Tm r',Tm r])          [] (name d++"["++name (source d)++"*"++name (target d)++"] is surjective")     sgn (nr d) ""
            h Inj = Ru Implication (F [Tm r,Tm r']) (pos d) id           [] (name d++"["++name (source d)++"*"++name (target d)++"] is injective")      sgn (nr d) ""
            h Tot = Ru Implication id (pos d) (F [Tm r,Tm r'])           [] (name d++"["++name (source d)++"*"++name (target d)++"] is total")          sgn (nr d) ""
            sgn   = (source d,source d)
            r     = Mph (name d)                (pos d) [] (source d,target d) True d
            r'    = flp (r ) 
            r'' t = Mph (t++"["++(name d)++"]") (pos d) [] (source d,target d) True d
            id    = F [Tm (I [source d] (source d) (source d) True)]
            id'    = F [Tm (I [target d] (target d) (target d) True)]


   instance Language Pattern where
    declaredRules pat = [r|r@(Ru c antc pos cons cpu expla sgn nr pn)<-ptrls pat]
    rules pat         = []
    signals pat       = [r|r@(Sg p rule expla sgn nr pn signal)<-ptrls pat]
    specs pat         = [r|r@(Gc pos m expr cpu sgn nr pn)<-ptrls pat]
    patterns pat      = [pat]
    isa pat           = Isa ts (singles>-[e| G pos g s<-ptgns pat,e<-[g,s]])
                        where Isa tuples singles = isa (ptdcs pat)
                              ts = clear (tuples++[(g,s)| G pos g s<-ptgns pat])


   instance Language Rule where
    declaredRules r@(Gc pos m expr cpu sgn nr pn)
     = [Ru Equivalence (F [Tm m]) pos expr cpu (name m++" is implemented using "++enumerate (map name (mors expr))) sgn nr pn]
    declaredRules   r@(Ru _ _ _ _ _ _ _ _ _) = [r]
    declaredRules   r                        = []
    rules r                                  = []
    signals r@(Sg _ _ _ _ _ _ _)             = [r]
    signals r                                = []
    specs   r@(Gc _ _ _ _ _ _ _)             = [r]
    specs   r                                = []
    patterns r                               = [Pat "" [r] [] [] [] []]
    isa (Gc _ m expr _ _ _ _)
      = Isa tuples (concs expr>-[e|(a,b)<-tuples,e<-[a,b]])
        where tuples = clear [(source expr,source m),(target expr,target m)]
    isa r = empty

   clear abs = rd [(a,b)| (a,b)<-abs, a/=b]

