{-# OPTIONS_GHC -Wall #-}
module Classes.ViewPoint (ViewPoint(..)) 
where
   import Adl.Context                 (Context(..))
   import Adl.Pattern                 (Pattern(..))
   import Adl.Gen                     (Gen(..))
   import Adl.Rule                    (Rule(..), rulefromProp, ruleviolations)
   import Adl.ObjectDef               (ObjectDef(..))
   import Adl.KeyDef                  (KeyDef(..))
   import Adl.MorphismAndDeclaration  (Declaration,mIs)
   import Adl.Concept                 (Concept(..),Morphic(..))
   import Adl.ConceptDef              (ConceptDef)
   import Adl.Expression              (Expression(..))
   import Adl.Pair                    (Paire)
   import Adl.FilePos                 (Numbered(..),FilePos(..))
   import Classes.Morphical           (Morphical(..))
   import Collection                  (Collection(..))
   import CommonClasses               (Identified(..))
   import Typology                    (Inheritance(..))

   class Morphical a => ViewPoint a where
     objectdef    :: a -> ObjectDef     -- The objectdef that characterizes this viewpoint
     conceptDefs  :: a -> [ConceptDef]  -- all concept definitions that are valid within this viewpoint
     declarations :: a -> [Declaration] -- all relations that have a valid declaration in this viewpoint. (Don't confuse declarations with decls, which gives the relations that are used in a. The function decls is bound in Morphical.)
     rules        :: a -> [Rule]        -- all rules that are maintained within this viewpoint,
                                        --   which are not signal-, not multiplicity-, and not key rules.
     signals      :: a -> [Rule]        -- all signals that are visible within this viewpoint
                                        -- all relations used in signals and rules must have a valid declaration in the same viewpoint.
     multrules    :: a -> [Rule]        -- all multiplicityrules that are maintained within this viewpoint.
     multrules x = [rulefromProp p d |d<-declarations x, p<-multiplicities d]
     objDefs      :: a -> [ObjectDef]
     keyDefs      :: a -> [KeyDef]      -- all keys that are defined in a
     gens         :: a -> [Gen]         -- all generalizations that are valid within this viewpoint
     patterns     :: a -> [Pattern]     -- all patterns that are used in this viewpoint
     isa          :: a -> Inheritance Concept
     --TODO -> there are more rules than rules+multrules that can be violated
     violations   :: a -> [(Rule,Paire)] --the violations of rules and multrules of this viewpoint
     violations x = [(r,viol) |r<-(rules x) ++ (multrules x), viol<-ruleviolations r] 

   instance ViewPoint a => ViewPoint [a] where
    objectdef _      = Obj { objnm   = ""         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                           , objpos  = Nowhere    -- ^ position of this definition in the text of the ADL source file (filename, line number and column number)
                           , objctx  = Tm (mIs S) -- ^ this expression describes the instances of this object, related to their context. 
                           , objats  = []         -- ^ the attributes, which are object definitions themselves.
                           , objstrs = []         -- ^ directives that specify the interface.
                           }
    conceptDefs xs   = (concat. map conceptDefs) xs
    declarations xs  = (rd . concat. map declarations) xs
    rules xs         = (concat. map rules) xs
    signals xs       = (concat. map signals) xs
    multrules xs     = (concat. map multrules) xs
    objDefs xs       = (concat . map objDefs) xs
    keyDefs xs       = (concat . map keyDefs) xs
    gens xs          = (rd . concat. map gens) xs
    patterns         = rd' name.concat.map patterns -- TODO: nagaan waar wordt afgedwongen dat elk pattern door zijn naam identificeerbaar is.
    isa              = foldr uni empty.map isa
    violations xs    = (concat. map violations) xs

   instance ViewPoint Context where
    objectdef    context = Obj { objnm   = name context
                               , objpos  = Nowhere
                               , objctx  = Tm (mIs S)
                               , objats  = map objectdef (ctxpats context)
                               , objstrs = []
                               }
    conceptDefs  context = ctxcs context++conceptDefs (ctxpats context)
    declarations context = declarations (ctxpats context) `uni` ctxds context
    rules        context = rules   (ctxpats context) ++ [r| r<-ctxrs context, not (isSignal r)]
    signals      context = signals (ctxpats context) ++ [r| r<-ctxrs context,      isSignal r] 
    objDefs      context = ctxos   context
    keyDefs      context = rd$keyDefs (ctxpats context) ++ ctxks context -- TODO: Hoe wordt gezorgd dat de keys uniek identificeerbaar zijn?
    gens         context = gens (ctxpats context)
    patterns     context = ctxpats context
    isa          context = ctxisa  context

   instance ViewPoint Pattern where
    objectdef    pat = Obj { objnm   = name pat
                           , objpos  = Nowhere
                           , objctx  = Tm (mIs S)
                           , objats  = []
                           , objstrs = []
                           }
    conceptDefs  pat = ptcds pat
    declarations pat = ptdcs pat `uni` declarations (ptrls pat)
    rules        pat = [r|r<-ptrls pat, not (isSignal r)]
    signals      pat = [r|r<-ptrls pat,      isSignal r ]
    objDefs       _  = []
    keyDefs      pat = ptkds pat
    gens         pat = ptgns pat
    patterns     pat = [pat]
    isa          pat = Isa ts (concs pat>-[c| g<-ptgns pat,c<-[gengen g,genspc g]])
                       where ts = clear [(gengen g,genspc g)| g<-ptgns pat]

   instance ViewPoint Rule where
    objectdef rule = Obj { objnm   = name rule
                         , objpos  = pos rule
                         , objctx  = Tm (mIs S)
                         , objats  = []
                         , objstrs = []
                         }
    conceptDefs  _ = []
    declarations r = [srrel r]
    rules        r = [r| not (isSignal r)]
    signals      r = [r| isSignal r]
    objDefs      _ = []
    keyDefs      _ = []
    gens         _ = []
    patterns r = [Pat{ ptnm  = ""
                     , ptrls = [r]
                     , ptgns = [G Nowhere g s ""|g<-concs r, s<-concs r, g<s, null [x| x<-concs r>-[g,s], g<x, x<s]]
                     , ptdcs = []
                     , ptcds = []
                     , ptkds = []
                     , ptxps = []}]
    isa r      = Isa ts (concs r>-[c| (g,s)<-ts,c<-[g,s]])
                 where ts = [(g,s)| g<-concs r, s<-concs r, g<s, null [c|c<-concs r, g<c, c<s]]
-- was    isa r = empty

   clear :: [(Concept,Concept)] -> [(Concept,Concept)]
   clear abs' = rd [(a,b)| (a,b)<-abs', a/=b]

