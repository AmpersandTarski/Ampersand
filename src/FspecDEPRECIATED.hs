   module FspecDEPRECIATED (
                   projectClassic
                  ,generateFspecLaTeX
                  ,generateArchLaTeX
                  ,generateGlossaryLaTeX
                  ,funcSpec
           --       ,nServices
           --       ,nFpoints
                           )
  where
   import FPA
   import Char
   import CommonClasses ( Identified(..))
   import Collection (Collection (isc,(>-),empty, rd))
   import Auxiliaries
          (  unCap, upCap, firstCaps
           , fst3, snd3
           , chain
           , showL
           , commaEng
           , commaNL
           , sort
           , eqCl 
           , eqClass
           , rd'
           , sort'
           , sord'
           , fixSpaces
           , haskellIdentifier
          )
   import Classification
    (  Classification(Cl, Bottom)
      , preCl
     )
   import Languages(Lang(Dutch,English),ShowLang(showLang),plural)
   import Typology
   import ADLdef
   import ShowADL
   import ShowHS
   import CC_aux(  isSgn
                 , applyM ,conts, explain, fEmpty
                 )
   import Calc
   import PredLogic
   import HtmlFilenames
   import LaTeX

-- WAAROM? Stef, deze module moet worden uitgekleed. De functies die er in zitten, werken voornamelijk nog op de oude manier, dus direct vanaf Context. Dit moet natuurlijk Fspc worden, en dan mag er ook een eigen module per functie worden aangemaakt.

   data Fspec = Fctx Context [Ftheme] [Dataset] [Fobj] [Frule]

--   instance ShowHS Fspec where
--    showHSname (Fctx context themes datasets objects vrules) = "f_Ctx_"++haskellIdentifier (name context)
--    showHS indent fctx@(Fctx context themes datasets objects vrules)
--     = "Fctx "++showHSname context++"   -- context   (Fspec has this structure:  Fctx context themes datasets objects vrules)"++
--       (if null themes   then " []" else indent++"{- themes   -}  "++showL [showHSname t|t<-themes  ])++
--       (if null datasets then " []" else indent++"{- datasets -}  "++showL [showHSname d|d<-datasets])++
--       (if null objects  then " []" else indent++"{- objects  -}  "++showL [showHSname o|o<-objects ])++
--       (if null vrules   then " []" else indent++"{- objects  -}  "++showL [showHSname o|o<-vrules  ])++
--       indent++"where"++
--       indent++" gE = genE "++showHSname context++
--       "\n>-- ***VIEWS***: " ++
--       (if null objects            then "" else concat [indent++" "++showHSname o++indent++"  = "++showHS (indent++"    ") o|o<-objects ]++"\n")++
--        "\n>-- ***RULES***: "++
--       (if null vrules             then "" else concat [indent++" "++showHSname r++indent++"  = "++showHS (indent++"    ") r|r<-vrules  ]++"\n")++
--        "\n>-- ***DATASETS***: "++
--       (if null datasets           then "" else concat [indent++" "++showHSname d++indent++"  = "++showHS (indent++"    ") d|d<-datasets]++"\n")++
--        "\n>-- ***THEMES***: "++
--       (if null themes             then "" else concat [indent++" "++showHSname t++         " = "++showHS (indent++"    ") t|t<-themes  ]++"\n")++
--        "\n>-- ***PATTERNS***: "++
--       (if null (ctxpats context) then "" else concat ["\n\n>  "++showHSname pat++" gE"++"\n>   = "++showHS "\n>     " pat|pat<-ctxpats context]++"\n")

 -- The story:
 -- A number of datasets for this context is identified.
 -- Every pattern is considered to be a theme and every object is treated as a separate object specification.
 -- Every dataset is discussed in precisely one theme
 -- Every theme will be explained in a chapter of its own.

--   makeFspecOUD :: Context -> Fspec
--   makeFspecOUD context
--     = Fctx ( context { ctxpats = ctxpats context ++ if null remainingDS then [] else [others]
--                      , ctxrs   = rules context
--                      , ctxds   = declarations context
--                      , ctxcs   = conceptDefs context
--                      , ctxks   = []
--                      , ctxos   = attributes context
--                      }
--            ) 
--            (  [makeFtheme context pat ds| (pat,ds)<-pats]                      -- one pattern yields one theme
--            ++ [makeFtheme context others remainingDS| not (null remainingDS)]  -- remaining datasets are discussed at the end
--            )
--            datasets
--            [ makeFview context o | o <-attributes context]
--            [ makeFrule context r | r <-rules context]
--       where
--        datasets = makeDatasets context
--
-- -- next thing, we look which datasets will be discussed in which themes.
-- -- Priority is given to those patterns that contain a concept definition of a root concept of the dataset,
-- -- because we assume that the programmer found it important enough to define that concept in that pattern.
-- -- in order to ensure that at most one pattern discusses a dataset, double (pat,cs,d)-triples are dropped.
--        pcsds0 = (map (head.sort' snd3).eqCl (name.fst3))
--                 [ (pat,length cns,ds)
--                 | pat<-ctxpats context, ds<-datasets, cns<-map name (concs ds) `isc` [name c|c<-conceptDefs pat], not (null cns)]
-- -- Now, pcsds0 covers concepts that are both root of a dataset and are defined in a pattern.
-- -- The remaining concepts and datasets are determined in pcsds1.
-- -- A dataset is assigned to the pattern with the most morphisms about the root(s) of the dataset.
--        pcsds1 = (map (head.sort' snd3).eqCl (name.fst3))
--                 [ (pat,0-length ms,ds)
--                 | pat<-ctxpats context, ds <- datasets>-[ds|(_,_,ds)<-pcsds0]
--                 , ms<-[[m|m<-morlist pat, m `elem` mors ds || flp m `elem` mors ds]], not (null ms)
--                 ]
-- -- The remaining datasets will be discussed in the last theme
--        remainingDS = datasets>-[ds'|(_,_,ds')<-pcsds0++pcsds1]
--        others
--         = Pat "Other topics" rs gen pms cs ks
--           where rs  = []
--                 gen = []
--                 pms = rd [d| ds<-remainingDS, d<-declarations ds]
--                 cs  = []
--                 ks  = []
--   --     context' = Ctx nm on i world pats rs ds cs ks os pops
--   --        where nm    = name context
--   --              on    = extends context
--   --              i     = isa context
--   --              world = wrld context
--   --              pats  = ctxpats context ++ if null remainingDS then [] else [others]
--   --              rs    = rules context
--   --              ds    = declarations context
--   --              cs    = conceptDefs context
--   --              ks    = []
--   --              os    = attributes context
--   --              pops  = populations context
-- -- The patterns with the appropriate datasets are determined:
--        pats = [ (pat, [dg| (p,_,dg)<-pcsds0++pcsds1, name pat==name p]) | pat<-ctxpats context]

   instance Identified Dataset where
     name (DS c pths) = name c
     name (BR m) = name m
     typ d = error "No Dataset typ defined"
     
   makeDatasetsOUD :: Context -> [Dataset]
   makeDatasetsOUD context
    = [ DS (minimum [g|g<-concs context,g<=head cl]) (dss cl)
      | cl<-eqClass bi (concs context) ]
      where
       c `bi` c' = not (null [m| m<-declarations context, isFunction m, isFunction (flp m)
                               , source m<=c && target m<=c'  ||  source m<=c' && target m<=c])
       dss cl = [     makeMph d | d<-declarations context, isFunction      d , source d `elem` cl]++
                [flp (makeMph d)| d<-declarations context, isFunction (flp d), target d `elem` cl]


   data Frule = Frul Rule

--   instance ShowHS Frule where
--    showHSname (Frul r)    = "frule_"++showHSname r     -- (name r)
--    showHS indent (Frul r) = "Frul (" ++showHS "" r++")"

--   makeFruleOUD :: Context -> Rule -> Frule
--   makeFruleOUD context r = Frul r






   data Dataset = DS Concept     -- the root of the dataset
                     [Morphism]  -- the functions from the root
                | BR Morphism    -- for every m that is not (isFunction m || isFunction (flp m))

--   instance ShowHS Dataset where
--    showHSname (DS c pths) = "f_DS_"++haskellIdentifier (name c)
--    showHSname (BR m)      = "f_BR_"++haskellIdentifier (name m++name (source m)++name(target m))
--    showHS indent (DS c  [] ) = "DS ("++showHS "" c++") []"
--    showHS indent (DS c pths) = "DS ("++showHS "" c++")"++indent++"   [ "++chain (indent++"   , ") [showHS (indent++"     ") pth| pth<-pths]++indent++"   ]"
--    showHS indent (BR m     ) = "BR ("++showHS "" m++")"
   instance Eq Dataset where  -- opletten: een dataset moet één vast concept hebben waaraan het wordt herkend.
    DS c _ == DS d _ = c==d
    BR m   == BR m'  = m==m'
    _      == _      = False




   dataset :: Context -> Concept -> Dataset
   dataset context c
    = DS (minimum [g|g<-concs context,g<=head cl]) dss
      where
       cl   = head ([cl| cl<-eCls, c `elem` cl]++error ("!Fatal (module Fspec>dataset): cannot determine dataset for concept "++name c))
       eCls = eqClass bi (concs context)
       c `bi` c' = not (null [m| m<-declarations context, isFunction m, isFunction (flp m)
                               , source m<=c && target m<=c'  ||  source m<=c' && target m<=c])
       dss = [     makeMph d | d<-declarations context, isFunction      d , source d `elem` cl]++
             [flp (makeMph d)| d<-declarations context, isFunction (flp d), target d `elem` cl]
















   instance Morphical Dataset where
    concs        (DS c pths) = concs pths
    concs        (BR m     ) = concs m
    conceptDefs  (DS c pths) = []
    conceptDefs  (BR m     ) = []
    mors         (DS c pths) = pths
    mors         (BR m     ) = [m]
    morlist      (DS c pths) = pths
    morlist      (BR m     ) = [m]
    declarations (DS c pths) = declarations pths
    declarations (BR m     ) = declarations m
    genE         (DS c pths) = genE c
    genE         (BR m     ) = genE m
    closExprs    (DS c pths) = []
    closExprs    (BR m     ) = []

   data Fobj  = Fobj Dataset ObjectDef [ServiceSpec] [Frule]

--   instance ShowHS Fobj where
--    showHSname (Fobj dset objd svcs rs) = "f_Obj_"++haskellIdentifier (name objd)
--    showHS indent (Fobj dset objd svcs rs)
--     = "Fobj "++showHSname dset
--       ++indent++" -- objectdef NOG UIT TE WERKEN IN FspecDef.lhs " 
--       ++indent++"     ("++showHS (indent++"      ") objd++")"
--       ++indent++" -- services NOG UIT TE WERKEN IN FspecDef.lhs "
--       ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") svc| svc<-svcs]++indent++"     ]"
--       ++indent++" -- rules NOG UIT TE WERKEN IN FspecDef.lhs " 
--       ++indent++"     ["++chain ", " [showHSname r| r<-rs]++"]"
--       ++indent++" -- Einde Fobj "++showHSname dset






--   makeFviewOUD :: Context -> ObjectDef -> Fobj
--   makeFviewOUD context o
--    = Fobj (dataset context (concept o)) o
--           ([ getEach context o
--            , createObj context o [] {-rs-}
--            , readObj context o
--            , deleteObj context o [] {-rs-}
--            , updateObj context o [] {-cs-} [] {-rs-} ])
--           [Frul r| r<-rules context, not (null (mors r `isc` mors o))]  -- include all valid rules that relate directly to o.




   data Ftheme  = Tspc Pattern [Funit]

--   instance ShowHS Ftheme where
--    showHSname (Tspc pat us) = "f_Thm_"++haskellIdentifier (name pat)
--    showHS indent (Tspc pat us) = "Tspc ("++showHSname pat++" gE)"++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") u| u<-us]++indent++"     ]"





--   makeFthemeOUD :: Context -> Pattern -> [Dataset] -> Ftheme
--   makeFthemeOUD context pat dss
--    = Tspc pat [makeFunit context pat (objs ds) [] []| ds<-dss]
--      where
--       objs ds = [o| o<-attributes context, dataset context (concept o)==ds]

   data Funit = Uspc String Pattern
                     [(ObjectDef,FPA,[Morphism],[(Expression,Rule)])]
                     [ServiceSpec] -- services

--   instance ShowHS Funit where
--    showHSname (Uspc nm pat ents svs) = "f_Unit_"++haskellIdentifier nm
--    showHS indent (Uspc nm pat ents svs)
--     = "Uspc "++show nm++" ("++showHSname pat++" gE)"
--       ++indent++"     [ "++chain (indent++"     , ") ["(" ++ showHS (indent++"       ") o ++ ", ILGV Eenvoudig, [] {-cs-},[] {-rs-}) "     | (o,fpa,cs,rs)<-ents]++indent++"     ]"
--       ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") s| s<-svs ]++indent++"     ]"

   makeFunitOUD :: Context -> Pattern -> [ObjectDef] -> [Concept] -> [ServiceSpec] -> Funit
   makeFunitOUD context pat objs newConcs newDecls
    = Uspc (if null objs then "" else name (head objs)) pat 
          objDefs
          servicespecs
            where
              mkobjdf :: ObjectDef -> (ObjectDef,FPA,[Morphism],[(Expression,Rule)])
              objDefs = [mkobjdf o| o<-objs]
              mkobjdf o = (o, ILGV Eenvoudig,  [] {-cs-},[] {-rs-})
              servicespecs = []
           --  (concat [ [ createObj context o [] {-rs-} ] ++ [ readObj context o]                           ++
           --            concat [ [keyEnt context o (key,ks), delKeyEnt context o (key,ks) [] {-rs-}]
           --                   | (e,key,ks)<-keys pat, e==concept o]                                ++
           --            [ deleteObj context o [] {-rs-} ]                                                     ++
           --            [ updateObj context o [] {-cs-} [] {-rs-}| not (null [] {-cs-}) ]
           --          | o<-objs ])






   data ServiceSpec = Sspc String       -- name of the service
                           [Morphism]   -- the list of relations this service may see
                           [Morphism]   -- the list of relations this service may change
                           FPA          -- function point analysis information
                           [ParamSpec]  -- parameters
                           [ParamSpec]  -- results
                           [Rule]       -- Invariants
                           [String]     -- Preconditions
                           [String]     -- Postconditions
   data ParamSpec   = Aspc String       -- name of the parameter
                           String       -- type of the parameter
                    | Pbool

--   instance ShowHS ServiceSpec where
--    showHSname (Sspc nm sees changes fpa input output rs pre post) = "f_svc_"++haskellIdentifier nm
--    showHS indent (Sspc nm sees changes fpa input output rs pre post)
--     = "Sspc "++show nm
--       ++indent++"     [ " ++chain (indent++"     , ") (map (showHS (indent++"       ")) sees   )++indent++"     ] -- these are the visible morphisms: <sees> "
--       ++indent++"     [" ++(if null changes then "]   -- no relations will be changed"  else " "++chain (indent++"     , ") (map (showHS (indent++"       ")) changes)++indent++"     ] -- these are the morphisms that may be altered: <changes> ")
--       ++indent++"     (" ++show fpa++")"
--       ++indent++"     [" ++(if null input   then "]   -- there are no input parameters" else " "++chain "," (map (showHS "") input )++"] -- these are the input parameters: <input>")
--       ++indent++"     [" ++(if null output  then "]   -- no output parameters"          else " "++chain "," (map (showHS "") output)++"] -- these are the output parameters: <output> ")
--       ++indent++"     [" ++(if null rs      then "]   -- there are no rules"            else " "++chain (indent++"     , ") (map (showHS (indent++"       ")) rs  )++indent++"     ]")
--       ++indent++"     [" ++(if null pre     then "]   -- there are no preconditions"    else " "++chain (indent++"     , ") (map  show                        pre )++indent++"     ] -- preconditions")
--       ++indent++"     [" ++(if null post    then "]   -- there are no postconditions"   else " "++chain (indent++"     , ") (map  show                        post)++indent++"     ] -- postconditions")

--   instance ShowHS ParamSpec where
--    showHSname a@(Aspc nm typ) = error ("(module Fspec) should not showHSname the ParamSpec (Aspc): "++showHS "" a)
--    showHS indent (Aspc nm typ)
--     = "Aspc "++show nm++" "++show typ





















   namet :: Identified a => a -> String
   namet    = firstCaps.map toLower.name
   nameAt :: (Identified a, Object a) => a -> String
   nameAt a = firstCaps ((map toLower.name.target.ctx) a++"_"++name a)
   nameAtt a = tt (nameAt a)
   getEach :: Context -> ObjectDef -> ServiceSpec 
   getEach context o
    = Sspc (firstCaps ("getEach"++firstCaps (name o)))
           [mIs (concept o)]  -- see  concept o  only.
           []                 -- change nothing
           (IF Eenvoudig)
           [ ]                                          -- input parameters
           [ Aspc "objs" ("["++handle context o++"]")]  -- results
           []                                           -- rules
           []                                           -- Precondition
           ([tt "objs"++"= I["++name (concept o)++"]"]) -- Postcondition
      where varName = uName (map name (attributes o))
   createObj :: Context -> ObjectDef -> [(Expression,Rule)] -> ServiceSpec 
   createObj context o rs
    = Sspc (firstCaps ("create"++firstCaps (name o)))
           ([mIs (concept o)]++mors o)  -- see the morphisms touched by this object
           (mors o)                     -- change these morphisms
           (IF Gemiddeld)
           [ Aspc (varName (name a)) (handle context a) | a<-attributes o]  -- input parameters
           [ Aspc "obj" (handle context o)]                                 -- results
           (dressRules
           [ (clause,rule)
           | (conj,rule)<-rs
           , clause@(Fu terms)<-[lClause conj]
           , not (null (mors o `isc` mors [t| Cp t<-terms]))])
 --    Precondition
           []
 --    Postcondition (example:) {o in Pair and o left l and o right r}
           ([tt ("obj."++name a)++"="++tt (varName (name a))|a<-attributes o])
      where varName = uName (map name (attributes o))
   readObj :: Context -> ObjectDef -> ServiceSpec
   readObj context o
    = Sspc (firstCaps ("read"++name o))
           ([mIs (concept o)]++mors o) -- sees
           []                          -- change nothing
           (OF Eenvoudig)
           [Aspc "x" (handle context o)]
           [Aspc (varName (name a)) (handle context a) | a<-attributes o]
           []
 --    Pre (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
           ([ tt ("x."++name a)++"="++idNam (nameAt a) |a<-attributes o])
 --    Post (example:) {left=l, right=r, src=s, and trg=t}
           [tt (varName (name a))++"="++idNam (nameAt a)|a<-attributes o]
      where varName = uName (map name (attributes o))
   keyEnt :: Context -> ObjectDef -> (String,[ObjectDef]) -> ServiceSpec
   keyEnt context o (key,ats')
    = Sspc (firstCaps ("sel"++name o++"_by_"++if null key then chain "_" (map name ats') else key))
           (mors context) -- see everything
           []             -- change nothing
           (OF Eenvoudig)
           [ Aspc (varName (name a)) (handle context a) | a<-ats']
           [Aspc "obj" (handle context o)]
           []
 --    Pre (example:) {Assume l=atom_left and r=atom_right}
           [let args = [tt ("x."++name a)++"="++tt (varName (name a)) | a<-attributes o] ++
                       [tt ("x."++name a)++"="++idNam (nameAt a)      | a<-attributes o, not (name a `elem` map name ats')]
            in
            "\\hbox{There is an {\\tt x}}\\in"++idName (concept o)++"\\ \\hbox{such that}"++
            (if length args==1 then "\\ "++concat args else
             "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
             chain "\\\\\n" (map ('&':) args)++
             "&)\n\\end{array}$"
            )]
 --    Post (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
           ([ tt "obj"++"="++tt "x"])
      where varName = uName (map name (attributes o))
   delKeyEnt :: Context -> ObjectDef -> (String,[ObjectDef]) -> [(Expression,Rule)] -> ServiceSpec
   delKeyEnt context o (key,ats') rs
    = Sspc (firstCaps ("del"++name o++"_by_"++if null key then chain "_" (map name ats') else key))
           (mors context)     -- see everything
           (mors context)     -- change everything
           (IF Gemiddeld)
           [ Aspc (varName (name a)) (handle context a) | a<-ats']
           []
           (dressRules
           [ (clause,rule)
           | (conj,rule)<-rs
           , clause@(Fu terms)<-[rClause conj]
           , not (null (mors o `isc` mors [t| t<-terms, isPos t]))])
 --    Pre (example:) 
           []
 --    Post (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
           ["\\hbox{\\tt obj}\\in"++idName o++"\\ \\hbox{implies that not}"++
            (if length (attributes o)==1 then let a=head (attributes o) in "\\ ("++tt ("obj."++name a)++"="++showADL (ctx a)++")" else
            "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
            chain "\\\\\n" ["&"++tt ("obj."++name a)++"="++tt (varName (name a))|a<-attributes o]++
            "&)\n\\end{array}$"
            )]
      where varName = uName (map name (attributes o))

   updateObj :: Context -> ObjectDef -> [Morphism] -> [(Expression,Rule)] -> ServiceSpec
   updateObj context o cs rs
    = Sspc (firstCaps ("update"++name o))
           (mors o)     -- sees
           (mors o)     -- changes
           (IF Gemiddeld)
           (Aspc "x" (handle context o): [ Aspc (varName (name a)) (handle context a) | a<-attributes o])
           []
           (dressRules rs)
 --    Pre (example:) {Assume x left l, x right r, x src s, and x trg t}
           []
 --    Post (example:) {x left l, x right r, x src s, and x trg t}
           [ tt ("x."++name a)++"="++tt (varName (name a))|a<-attributes o]
      where varName = uName (map name (attributes o))

   deleteObj :: Context -> ObjectDef -> [(Expression,Rule)] -> ServiceSpec
   deleteObj context o rs
    = Sspc (firstCaps ("delete"++name o))
           [mIs (concept o)]     -- see everything
           (mors o)              -- change everything
           (IF Gemiddeld)
           [Aspc "x" (handle context o)] []
           (dressRules
           [ (clause,rule)
           | (conj,rule)<-rs
           , clause@(Fu terms)<-[rClause conj]
           , not (null (mors o `isc` mors [t| t<-terms, isPos t]))])
 --    Pre
           [if length (attributes o)==1 then let a=head (attributes o) in tt ("x."++name a)++"="++idNam (nameAt a) else
            "$\\begin{array}[t]{lll}\n"++
            chain "\\\\\nand" ["&"++tt ("x."++name a)++"="++idNam (nameAt a)|a<-(attributes o)]++
            "&\n\\end{array}$"
           | not (null (attributes o))]
 --    Post 
           [if null (attributes o) then "\\hbox{\\tt x}\\not\\in"++idName (concept o) else
            "\\hbox{\\tt obj}\\in"++idName (concept o)++"\\ \\hbox{implies that not}"++
            (if length (attributes o)==1 then let a=head (attributes o) in "\\ ("++tt ("obj."++name a)++"="++idNam (nameAt a)++")" else
            "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
            chain "\\\\\nand" ["&"++tt ("obj."++name a)++"="++idNam (nameAt a)|a<-attributes o]++
            "&)\n\\end{array}$"
            )]

   newPair :: Context -> [Declaration] -> [(Expression,Rule)] -> Declaration -> String -> ServiceSpec
   newPair context relations clauses r nm
    = Sspc (firstCaps ("assoc"++"_"++nm))
           (mors context)     -- see everything
           [makeMph r]        -- change r
           (IF Gemiddeld)
           [ Aspc s (handle context (source r)), Aspc t (handle context (target r))] []
 --hierboven gebeurt iets grappigs. Zouden de objecten waar deze handles naar wijzen wel bestaan?
           (dressRules [ (clause,rule)
                       | (conj,rule)<-clauses
                       , clause@(Fu terms)<-[lClause conj]
                       , r `elem` map makeDeclaration (mors [t| Cp t<-terms])])
           [] ["("++s++","++t++")\\ \\in\\ "++idName r]
      where varName = uName (map name [source r, target r])
            s = if homogeneous r then tt "s" else (tt.varName.name.source) r
            t = if homogeneous r then tt "t" else (tt.varName.name.target) r
   isPair :: Context -> [Declaration] -> Declaration -> String -> ServiceSpec
   isPair context relations r nm
    = Sspc (firstCaps ("member_"++nm))
           (mors context)     -- see everything
           []                 -- change nothing
           (OF Eenvoudig)
           [Aspc "s" (handle context (source r)),Aspc "t" (handle context (target r))] [Pbool] []
           [] [tt (firstCaps ("member_"++nm)++"("++s++","++t++")")++"\\ \\hbox{yields true iff}\\ ("++tt s++","++tt t++")\\in"++idName r]
      where varName = uName (map name [source r, target r])
            s = if homogeneous r then "s" else (varName.name.source) r
            t = if homogeneous r then "t" else (varName.name.target) r
   delPair :: Context -> [Declaration] -> [(Expression,Rule)] -> Declaration -> String -> ServiceSpec
   delPair context relations clauses r nm
    = Sspc (firstCaps ("remove"++"_"++nm))
           (mors context)     -- see everything
           [makeMph r]        -- change r
           (IF Gemiddeld)
           [Aspc s (handle context (source r)),Aspc t (handle context (target r))] []
           (dressRules
           [ (clause,rule)
           | (conj,rule)<-clauses
           , clause@(Fu terms)<-[rClause conj]
           , r `elem` map makeDeclaration (mors [t| t<-terms, isPos t])])
           [] [if isIdent r then s++"\\not ="++t else "("++s++","++t++")\\ \\not\\in\\ "++idName r]
      where varName = uName (map name [source r, target r])
            s = if homogeneous r then tt "s" else (tt.varName.name.source) r
            t = if homogeneous r then tt "t" else (tt.varName.name.target) r
   srcObjs :: Context -> [Declaration] -> Declaration -> String -> ServiceSpec
   srcObjs context relations r nm
    = Sspc srvName
           (mors context)     -- see everything
           []                 -- change nothing
           (OF Eenvoudig)
           [Aspc s (handle context (source r))]
           [if isFunction r 
            then Aspc outName (handle context (target r))
            else Aspc outName ("\\{"++handle context (target r)++"\\}")] []
           []
           [if isFunction r
            then tt outName++"\\ =\\ "++t++", where "++(if isIdent r then s++"="++t else "("++s++","++t++")\\in"++idName r)
            else tt outName++"\\ =\\ \\{"++t++"|\\ "++(if isIdent r then s++"="++t else "("++s++","++t++")\\in"++idName r)++"\\}"]
      where srvName = if homogeneous(r) then firstCaps ("trg_"++nm) else
                      if isFunction r 
                      then firstCaps ((map toLower.name.target) r++"_"++nm)
                      else firstCaps ((plural English .map toLower.name.target) r++"_"++nm)
            outName | isFunction r                = t
                    | Uni `elem` multiplicities r = "handle"
                    | otherwise                   = "handles"
            varName = uName (map name [source r, target r])
            s = if homogeneous r then tt "s" else (tt.varName.name.source) r
            t = if homogeneous r then tt "t" else (tt.varName.name.target) r
   trgObjs :: Context -> [Declaration] -> Declaration -> String -> ServiceSpec
   trgObjs context relations r nm
    = Sspc srvName
           (mors context)     -- see everything
           []                 -- change nothing
           (OF Eenvoudig)
           [Aspc t (handle context (target r))]
           [if isFunction (flp r) 
            then Aspc outName (handle context (source r))
            else Aspc outName ("\\{"++handle context (source r)++"\\}")] []
           []
           [if isFunction (flp r)
            then tt outName++"\\ =\\ "++s++", where "++(if isIdent r then s++"="++t else "("++s++","++t++")\\in"++idName r)
            else tt outName++"\\ =\\ \\{"++s++"|\\ "++(if isIdent r then s++"="++t else "("++s++","++t++")\\in"++idName r)++"\\}"]
      where srvName = if homogeneous(r) then firstCaps ("src_"++nm) else
                      if isFunction r 
                      then firstCaps ((map toLower.name.source) r++"_"++nm)
                      else firstCaps ((plural English .map toLower.name.source) r++"_"++nm)
            outName | isFunction (flp r)          = s
                    | Inj `elem` multiplicities r = "handle"
                    | otherwise                   = "handles"
            varName = uName (map name [source r, target r])
            s = if homogeneous r then tt "s" else (tt.varName.name.source) r
            t = if homogeneous r then tt "t" else (tt.varName.name.target) r

   applyMLatex :: Declaration -> String -> String -> String
   applyMLatex (Sgn nm _ _ _ prL prM prR _ _ _ _ _) d c = if null (prL++prM++prR) then "$"++d++"$\\ "++firstCaps nm++"\\ $"++c++"$" else latexWord prL++"$"++d++"$"++latexWord prM++"$"++c++"$"++latexWord prR
   applyMLatex (Isn _ _)                            d c = "$"++d++"$ equals $"++c++"$"
   applyMLatex (Iscompl _ _)                        d c = "$"++d++"$ differs from $"++c++"$"
   applyMLatex (Vs _ _)                             d c = show True




   funcSpec :: Context -> Lang -> [Ftheme]
   funcSpec context language
    = [ Tspc pat 
             ([ Uspc (firstCaps (name o)) pat [ {- (o,ILGV Eenvoudig,cs,rs) -} ]
                     ({- [ createObj context o rs ] ++ [ readObj context o]                           ++
                         concat [ [keyEnt context o (key,ks), delKeyEnt context o (key,ks) rs]
                                | (e,key,ks)<-keys pat, e==concept o]                                ++
                         [ deleteObj context o rs ]                                                     ++
                         [ updateObj context o cs rs| not (null cs) ] -} []
                     )
              | (o,fpa,cs,rs)<-ec])-- ++clss pat ec newConcs++asss pat newDecls)
      | (pat,newConcs,newDecls)<-zip3 (ctxpats context) (firsts [] (map concs (ctxpats context))) (firsts [] (map declarations (ctxpats context)))
      , car<-[definedEnts context pat], not (null car), ec<-[ents car] ]
      where
       ents car = [ (o,if null (attributes o) then NO else ILGV Eenvoudig,cs,rs)
                  | o<-car
 -- selecteer alle geldende regels, die relevant zijn voor o. Bepaal daarvan de conjuncts, omdat elke conjunct waar moet blijven.
                  , rs<-[[(conj,rule) |rule<-rules context, not (null (mors o `isc` mors rule)), conj<-conjuncts rule]]
 -- selecteer de verzameling cs van attributen van o, die door functionele afhankelijkheden (ook genoemd: triggers) kunnen veranderen.
                  , cs<-[[a| a<-attributes o, not (null (declarations (ctx a) `isc` declarations affected))]], not (null cs)
                  ]
       affected :: [Morphism]
       affected = rd[m| hc@(CR (fOps, e, bOp, toExpr, frExpr, rule))<-hcs, m<-mors toExpr]
       clss pat ec new
        = [ Uspc (if language==English then "Other Classes" else "Andere Classes") pat car
                 [ service
                 | (o,_,_,rs)<-car
                 , service <- [ createObj context o rs ]                                                ++
                              [ keyEnt context o (key,ks) | (e,key,ks)<-keys pat, e==concept o] ++
                              [ deleteObj context o rs ]
                 ]
          ] where car = [ (o,NO,[],rs)
                        | o<-objDefs pat,  not (o `elem` attributes context), o `elem` new
                        , rs<-[[(conj,rule) |rule<-declaredRules pat, conj<-conjuncts rule, concept o `elem` concs conj]]
                        ]
       asss pat new
        = [ Uspc ((if language==English then "Associations of " else "Associaties van ")++firstCaps (name pat)) pat [] ss
          | ss<-[[ service
                 | r<-relations, not (isSignal r), r `elem` new
                 , nm <- [name r++if length [d|d<-relations, name d==name r]==1 then "" else
                                  name (source r)++name (target r)]
                 , rs<-[[(conj,rule) |rule<-rules context, conj<-conjuncts rule, r `elem` declarations conj]]
                 , service <- [ newPair context relations rs r nm
                              , isPair context relations r nm
                              , delPair context relations rs r nm
                              , srcObjs context relations r nm
                              , trgObjs context relations r nm ]
                 ]]
          , not (null ss)
          ]
       relations = declarations context
       hcs = [hc| rule<-rules context, hc<-triggers rule ]

   funcSpecText context fspcs English
    = "Functional Specification:\n"++chain "\n\n" (map fSpec fspcs)
      where
       fSpec (Tspc pat units)
        = "Chapter "++firstCaps (name pat)++"\n\n"++chain "\n\n" (map fUnit units)
       fUnit unit@(Uspc unm pat car specs)
        = "Section "++unm++fpaUnit unit English++
            chain "\n\n"
            [ ( if null pre then "" else "\n  {Assume: "++chain " and " pre ++"}") ++
              nm++"("++chain ";" [p++":"++c| Aspc p c<-input]++")"++chain ";" [" "++p++" "++c| Aspc p c<-output]++
              ( if null post then "" else "\n  {"++chain " and " post ++"}") ++
              ( if null rs then "" else
                if length rs>1
                then "\n\tInvariants:\n   "++chain "\n   " [fixSpaces 3 (show (runum r))++") "++showOO r |r<-rs]
                else "\n\tInvariant: "++showOO (head rs) ++ "(Rule "++show (nr (head rs))++")"
              )++
              if fpa==NO then "" else
              "\n\n\tThis service is qualified in the FPA as "++showLang English (complexity fpa)++"."
            | Sspc nm sees changes fpa input output rs pre post<-specs
            ]

   funcSpecText context fspcs Dutch
    = "Functionele Specificatie:\n"++chain "\n\n" (map fSpec fspcs)
      where
       fSpec (Tspc pat units)
        = "Hoofdstuk "++firstCaps (name pat)++"\n\n"++chain "\n\n" (map fUnit units)
       fUnit unit@(Uspc unm pat car specs)
        = "Sectie "++unm++fpaUnit unit Dutch++
            chain "\n\n"
            [ ( if null pre then "" else "\n  {Stel: "++chain " and " pre ++"}") ++
              nm++"("++chain ";" [p++":"++c| Aspc p c<-input]++")"++chain ";" [" "++p++" "++c| Aspc p c<-output]++
              ( if null post then "" else "\n  {"++chain " and " post ++"}") ++
              ( if null rs then "" else
                if length rs>1
                then "\n\tInvarianten:\n   "++chain "\n   " [fixSpaces 3 (show (runum r))++") "++showOO r |r<-rs]
                else "\n\tInvariant: "++showOO (head rs) ++ "(Rule "++show (nr (head rs))++")"
              )++
              "\n\n\tDeze service is gekwalificeerd in de FPA als "++showLang Dutch (complexity fpa)++"."
            | Sspc nm sees changes fpa input output rs pre post<-specs
            ]










   fpaUnit unit@(Uspc unm pat car specs) language
    = (if length car==1
       then let (o,fpa,_,_) = head car in
            fpaText language unm (concept o) fpa++
            "\tFPA "++complex++":\n\n"++
            "\\begin{tabular}{|l|l|r|}\\hline concept&type&fp\\\\\\hline\n  "++
            chain "\\\\\n"
            [nm++"&"++showLang language fpa++"&"++show (fPoints fpa)
            | Sspc nm sees changes fpa input output rs pre post<-specs]++
            "\\\\\\hline\n\\end{tabular}\n\n"
       else "\tFPA "++complex++":\n\n"++
            "\\begin{tabular}{|l|l|r|}\\hline concept&type&fp\\\\\\hline\n  "++
            chain "\\\\\n"
            ([name o++"&"++showLang language fpa++"&"++show (fPoints fpa)
             | (o,fpa,_,_) <- car]++
             [nm++"&"++showLang language fpa++"&"++show (fPoints fpa)
             | Sspc nm sees changes fpa input output rs pre post<-specs])++
            "\\\\\\hline\n\\end{tabular}\n\n"
      )++
      (if length car+length specs<=1 then "" else
       "\t"++unm++aswhole++
       show (sum[fPoints fpa| (_,fpa,_,_)<-car]+sum [fPoints fpa| Sspc nm sees changes fpa input output rs pre post<-specs])++
       worth++".\n\n"
      )
      where
       fpaText language unm c NO = ""
       fpaText English unm c fpa
        = (if unm == name c then "\n\tThis concept" else "\n\tname c")++
          " is qualified in the FPA as a data collection of type "++showLang English fpa++".\n"++
          "\tIt is worth "++show (fPoints fpa)++" function points.\n\n"
       fpaText Dutch unm c fpa
        = (if unm == name c then "\n\tDit concept" else "\n\tname c")++
          " is gekwalificeerd in  FPA als een gegevensverzameling van type "++showLang Dutch fpa++".\n"++
          "\tDat levert "++show (fPoints fpa)++" functiepunten op.\n\n"
       complex | language==English = "complexity"
               | language==Dutch   = "complexiteit"
       aswhole | language==English = " as a whole is worth "
               | language==Dutch   = " als geheel is "
       worth   | language==English = " function points"
               | language==Dutch   = " functiepunten waard"

   funcSpecLaTeX context fspcs language
    = chain "\n\n" (map fSpec fspcs)
      where
       cname = name context
       fSpec (Tspc pat units)
        = latexChapter (firstCaps (name pat)) ("Ftheme "++firstCaps (name pat))++
          latexFigure (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_"++clname (name pat)++"_CD.png}")++
          captiontext language pat)++"\n"++
          ( if null attrs then "" else introtext language attrs++
            "\\begin{eqnarray}\n"++
            chain "\\\\\n" ["  "++idName d++"&:&"++idName (source d)++" \\times "++idName (target d)
                           | (a,d,cnm)<-attrs]++
            "\n\\end{eqnarray}\n"++
            intratext language attrs++
            "\\begin{eqnarray}\n"++
            chain "\\\\\n" [ "\\hbox{For each }"++tt cnm++"\\in"++idNam cnm++"&&"++






                             "\\label{sgn:"++firstCaps(name pat++":"++name d++name (source d)++name (target d))++"}"
                           | (a,d,cnm)<-attrs] ++
            "\n\\end{eqnarray}\n")++
  --      str4++
  --      "\\begin{tabular}{|lll|}\\hline relation&variables&fact\\\\\\hline\n  "++
  --      chain "\\\\\n" ["  "++firstCaps (name d)++"&"++v (source d)++",\\ "++idNam (v (target d))++"&"++applyMLatex d (v (source d)) (v (target d))
  --                     | d<-declarations new, v<-[tt.uName [name (source d),name (target d)].name] ]++
  --      "\\\\\\hline\n\\end{tabular}\n\n"++
          chain "\n\n" [fUnit u new| (u,new)<-(zip units.firsts [])
                                               [ [d| Sspc nm sees changes fpa input output rs pre post<-specs, d<-declarations rs
                                                   , isSgn d && not (isSignal d) && not (d `elem` map snd3 attrs)]
                                               | u@(Uspc unm pat car specs)<-units, not (null specs) ]
                       ]
          where
            varName = uName [name o| u@(Uspc unm pat car specs)<-units, (o,fpa,cs,rs)<-car]
            attrs = [ (a, d, varName (name o))
                    | u@(Uspc unm pat car specs)<-units 
                    , (o,fpa,cs,rs')<-car, a<-attributes o, d<-declarations a
                    ]

       isAttribute d = null ([Uni,Tot]>-multiplicities d) || null ([Sur,Inj]>-multiplicities d)
       fUnit unit@(Uspc unm pat car []) newdecs = ""
       fUnit unit@(Uspc unm pat car specs) newdecs
        = latexSection (firstCaps unm) ("Tspc "++firstCaps (unm++name pat))++
          fpaUnit unit language














       captiontext English pat
        = "\n\\caption{Data structure of "++(latexWord.name) pat++"}\n\\label{fig:"++clname (name pat)++"}"
       captiontext Dutch pat
        = "\n\\caption{Gegevensstructuur van "++(latexWord.name) pat++"}\n\\label{fig:"++clname (name pat)++"}"
       str2 | language==English = "The services are defined in the following subsections.\n"
            | language==Dutch   = "De services zijn in de volgende secties gedefinieerd.\n"
       lettext English "Other Classes"
        = "Services for other classes used in this chapter are defined here.\n"
       lettext English nm
        = if take 16 nm == "Associations of "
          then "To complete this chapter, we define services on associations between classes.\n"
          else "Let "++idNam nm++" be the set of all "++plural English (map toLower nm)++" in the system.\n"
       lettext Dutch "Andere Classes"
        = "Services voor andere classes die in dit hoofdstuk zijn gebruikt, worden in deze sectie gedefinieerd.\n"
       lettext Dutch nm
        = if take 16 nm == "Associaties van "
          then "Tenslotte volgen nog een aantal services op associaties tussen classes."
          else "Stel "++idNam nm++" is de verzameling van alle "++plural Dutch (map toLower nm)++" in het systeem.\n"
       introtext English attributes
        = if length attributes>1
          then "To describe the behaviour of the services defined in this chapter, we introduce one relation for each attribute:\n"
          else "To describe the behaviour of the services defined in this chapter, we introduce:\n"
       introtext Dutch attributes
        = if length attributes>1
          then "Om het gedrag te beschrijven van de services in dit hoofdstuk, introduceren we \\'e\\'en relatie voor elk attribuut:\n"
          else "Om het gedrag te beschrijven van de services in dit hoofdstuk, introduceren we:\n"
       intratext English attributes
        = "Attributes are defined in terms of these relations\n"
       intratext Dutch attributes
        = "Attributen zijn in termen van deze relaties gedefinieerd.\n"
       atttext English nm attributes
        = if length attributes>1
          then "The attributes of "++nm++" are defined in "
          else "The attribute of "++nm++" is defined in "
       atttext Dutch nm attributes
        = if length attributes>1
          then "De attributen van "++nm++" zijn gedefinieerd in "
          else "Het attribuut van "++nm++" is gedefinieerd in "

   firsts seen (ds:dss) = new: firsts (seen++new) dss where new = ds>-seen
   firsts seen [] = []
   handle :: Identified a => Context -> a -> String
   handle context c = firstCaps (name c)++if name c `elem` (map name (makeDatasetsOUD context)) then "Handle" else ""

   srvSchema pat language cnm (Sspc nm sees changes fpa input output rs pre post) new
    = latexSubsection nm nm++
      "{\\tt "++firstCaps nm++"}("++
      "\\begin{array}[t]{lr@{~:~}ll}\n"++
      ( let pars=["{\\tt In}&{\\tt " ++firstCaps p++"}&{\\tt "++c++"}"| Aspc p c<-input]++
                 ["{\\tt Out}&{\\tt "++firstCaps p++"}&{\\tt "++c++"}"| Aspc p c<-output]
        in if length pars==1 then concat pars++"\\ )" else chain "&;\\\\\n" pars++"&)"
      )++
      "\n\\end{array}\n"++
      str1++  -- When called, this service behaves as follows:
      "\\begin{tabular}{l}\n"++
      chain "\\\\\n" (["\\hspace{2em}\\{Pre: $"++
                                             "\\begin{array}[t]{ll}\n"++
                                             chain "\\hbox{and}\\\\\n" [p++"&"| p<-pre]++
                                             (if length pre>1 then "\\hspace{2em}\\}" else "\\}")++
                                             "\n\\end{array}$\n"| not (null pre)]++
 --                   ["\\hspace{2em}\\{Pre: $"++chain "\\ \\hbox{and}\\ " [p| p<-pre]++"$\\}"| not (null pre)]++
                      [call]++
                      ["\\hspace{2em}\\{Post: $"++
                                             "\\begin{array}[t]{ll}\n"++
                                             chain "\\hbox{and}\\\\\n" [p++"&"| p<-post]++
                                             (if length post>1 then "\\hspace{2em}\\}" else "\\}")++
                                             "\n\\end{array}$\n"| not (null post)]
                      )++"\n"++
      "\\end{tabular}\n"++
      -- For defining invariants, we introduce:
      ( if null new then "" else str3++  -- For defining invariants, we introduce:
        "\\begin{eqnarray}\n  "++
        chain "\\\\\n" ["  "++idNam (firstCaps (name d))++"&:&"++idName (source d)++" \\times "++idName (target d)++"\\label{sgn:"++firstCaps(cnm++":"++name d++name (source d)++name (target d))++"}"
                       | d<-declarations new ]++
        "\n\\end{eqnarray}\n\n"++
        if null facts then "" else
         if length facts == 1 then let (d,s,t,v)=head facts in "\tRelation "++firstCaps (name d)++"represents facts.\n\tExpression "++v s++" "++firstCaps (name d)++" "++v t++" means: "++applyM d (v s) (v t)++".\n" else
         str4++  -- These relations represent facts according to the following table
         "\n\\begin{tabular}{|lll|}\\hline relation&variables&fact\\\\\\hline\n  "++
         chain "\\\\\n" ["  "++firstCaps (name d)++"&"++v s++",\\ "++v t++"&"++applyMLatex d (v s) (v t)
                        | (d,s,t,v)<-facts]++
         "\\\\\\hline\n\\end{tabular}\n\n")++
      (if null rs then "" else
       if length rs>1
       then str5++  -- The service call "++call++" must maintain the following rules:
            "\\[\\begin{array}{l}\n   "++
            chain "\\\\\n   " [show (runum r)++")~~~"++(lshow language.assemble.normRule) r |r<-rs]++"\n"++
            "\\end{array}\\]\n"
       else "\n\nInvariant: \\("++(lshow language.assemble.normRule.head) rs++"\\)\\ "++str6++show (nr (head rs))++")\n")++
      (if null ms then "" else
       if length rs==1
       then str7++  -- Cross reference table to the relations used:
       "\\begin{tabular}{|lllrr|}\\hline\nname&source&target&def.&pg.\\\\\\hline\n   "++
       chain "\\\\\n   " [(idNam.firstCaps.name) d++"&"++(idNam.name.source) d++"&"++(idNam.name.target) d++"&"++"\\ref{sgn:"++firstCaps(name pat++":"++name d++name (source d)++name (target d))++"}&"++"\\pageref{sgn:"++firstCaps(name pat++":"++name d++name (source d)++name (target d))++"}"
                         |(d,rs)<-ms, isSgn d]++
       "\\\\\\hline\n\\end{tabular}\n"
       else str8++  -- Cross references to the relations used in these rules:
       "\\begin{tabular}{|lllrrl|}\\hline\nname&source&target&def.&pg.&used in\\\\\\hline\n   "++
       chain "\\\\\n   " [(idNam.firstCaps.name) d++"&"++(idNam.name.source) d++"&"++(idNam.name.target) d++"&"++"\\ref{sgn:"++firstCaps(name pat++":"++name d++name (source d)++name (target d))++"}&"++"\\pageref{sgn:"++firstCaps(name pat++":"++name d++name (source d)++name (target d))++"}&"++chain "," (map (show.nr) rs)
                         |(d,rs)<-ms, isSgn d]++
       "\\\\\\hline\n\\end{tabular}\n"
      )
      where
       facts = [ (d,s,t,tt.uName [s,t]
                 )
               | (d,ms)<-[(makeDeclaration (head cl), cl)| cl<-eqCl makeDeclaration new]
               , applyM d "" "" /= ""
               , s<-[if homogeneous d then "src_"++name (source d) else name (source d)]
               , t<-[if homogeneous d then "trg_"++name (target d) else name (target d)]
               ]
       ms = [ (snd (head cl), map fst cl)
            | cl<-eqCl snd [(r,d)| r<-rs, d<-declarations r]]
       call = "{\\tt "++firstCaps nm++"}("++chain "," ["{\\tt "++firstCaps p++"}"| Aspc p c<-input++output]++")"
       str1 | language==English = "\nWhen called, this service behaves as follows:\n\n"
            | language==Dutch   = "\nBij aanroep gedraagt deze service zich als volgt:\n\n"
       str3 | language==English = "\nFor defining invariants, we introduce:\n"
            | language==Dutch   = "\nVoor het defini\\\"eren van invarianten, introduceren we\n"
       str4 | language==English = "\nThese relations represent facts according to the following table:\n"
            | language==Dutch   = "\nDeze relaties representeren de volgende feiten:\n"
       str5 | language==English = "\n\nThe service call "++call++" must maintain the following rules:\n"
            | language==Dutch   = "\n\nDe aanroep "++call++" moet de volgende regels in stand houden:\n"
       str6 | language==English = "(Rule "
            | language==Dutch   = "(Regel "
       str7 | language==English = "\nCross reference table to the relations used:\n\n"
            | language==Dutch   = "\nKruisreferentietabel naar de gebruikte relaties:\n\n"
       str8 | language==English = "\nCross references to the relations used in these rules:\n\n"
            | language==Dutch   = "\nKruisreferenties naar de relaties die in deze regels zijn gebruikt\n\n"

   dressRules :: [(Expression,Rule)] -> [Rule]
   dressRules clauses = [ if length cl>1 then rule else makeRule rule clause | cl<-(map rd.eqCl snd) clauses, (clause,rule)<-take 1 cl]
    where
     f (Fu cl) (Fu cl') = [e| e<-cl, isPos e] `eq` [e| Cp e<-cl'] &&
                          [e| Cp e<-cl] `eq` [e| e<-cl', isPos e]
     self (Fu cl)       = [e| e<-cl, isPos e] `eq` [e| Cp e<-cl]
     a `eq` b = length a==length b && length a==length (a `isc` b)

   class LATEX a where
    lshow  :: Lang -> a -> String
    ltshow :: String -> Typology Concept -> Lang -> a -> String
    ltshow nm typ = lshow

   definingPattern context datasets c
    = ( fst . head . sort' ((0-).length.snd) ) [(p,rs)| (p,o,rs)<-ps, concept o==c]
      where
       ps = [(p,e,[r|(_,_,r)<-cl])| cl<-eqCl (\(p,o,r)->(name p,name o)) rs, (p,e,_)<-take 1 cl]
       rs = [(pat,o,rule)
            | pat<-ctxpats context, rule<-declaredRules pat, o<-attributes context, concept o `elem` concs rule]
   definedEnts context pat
    = [ e
      | cl<-eqCl (\(p,o,rs)->concept o) ps, (p,e,rs)<-take 1 (sort' (\(p,e,rs)->0-length rs) cl), p==name pat]
      where
       ps = [ (p,e,[r|(_,_,r)<-cl]) | cl <-eqCl (\(p,o,_)->(p,name o)) rs, (p,e,_)<-take 1 cl ]
       rs = [ (name pat,o,rule)
            | pat<-ctxpats context, rule<-declaredRules pat, o<-attributes context, concept o `elem` concs rule]

   nDesignPr context = n where (_,n) = dp undef f context where undef=undef; f=f
   designPrinciples English context = dps
    where (dps,_) = dp ( {- str1  -} "Design rules"
                       , {- str2  -} "This chapter discusses the rules that represent agreements between stakeholders about functionality."
                       , {- str3  -} "about"
                       , {- str4  -} "Concept"
                       , {- str5  -} "is defined in definition"
                       , {- str6  -} "on page"
                       , {- str7  -} "This section uses the following concepts:"
                       , {- str8  -} "The following concepts are defined previously in this chapter:"
                       , {- str9  -} "is used in this section, but is not yet defined in this document."
                       , {- str10 -} "Concepts"
                       , {- str11 -} "have not been defined in this document"
                       , {- str12 -} "A conceptual analysis about"
                       , {- str13 -} "is given in figure"
                       , {- str14 -} "Knowledge model of"
                       , {- str15 -} "relation"
                       , {- str16 -} "between"
                       , {- str17 -} "and"
                       )
                       (commaEng "and") context
   designPrinciples Dutch context = dps
    where (dps,_) = dp ( {- str1  -} "Ontwerpregels"
                       , {- str2  -} "Dit hoofdstuk bespreekt verschillende afspraken, die in volgende hoofdstukken zijn uitgewerkt tot een volledige functionele specificatie."
                       , {- str3  -} "over"
                       , {- str4  -} "Concept"
                       , {- str5  -} "is gedefinieerd in definitie"
                       , {- str6  -} "op pg."
                       , {- str7  -} "Deze sectie maakt gebruik van de volgende concepten:"
                       , {- str8  -} "De volgende concepten zijn eerder in dit hoofdstuk gedefinieerd:"
                       , {- str9  -} "wordt in deze sectie gebruikt, maar heeft nog geen definitie in dit document."
                       , {- str10 -} "De concepten"
                       , {- str11 -} "zijn in dit document nog niet gedefinieerd"
                       , {- str12 -} "Een conceptuele analyse over"
                       , {- str13 -} "is weergegeven in figuur"
                       , {- str14 -} "Kennismodel van"
                       , {- str15 -} "relatie"
                       , {- str16 -} "tussen"
                       , {- str17 -} "en"
                       )
                       (commaNL "en") context
  -- designPrinciples language = error ("Not yet implemented: designPrinciples "++show language++" (module Ftheme)")

   dp strs en context
     = ( (chain "\n". filter (not.null))
         ( [ latexChapter str1 str1
           , "\t"++str2
           ] ++
           patSections [] [] (ctxpats context)
         )
       , length [d| d<-declarations context, not (isSignal d)] +    -- bereken het totaal aantal requirements
         length (rules context++signals context)
       )
    where
     (str1,str2,str3,str4,str5,str6,str7,str8,str9,str10,str11,str12,str13,str14,str15,str16,str17) = strs
     patSections _ _ [] = []
     patSections prevWrittenConcepts prevWrittenDecls (pat:pats)
      = [latexSection (str1++" "++str3++" "++firstCaps (name pat)) (str1++name context++"Pat"++firstCaps (name pat))]++
        elaborate (rd(newConcsWithDefs>-prevWrittenConcepts))
                  [d| d<-declarations pat, not (isSignal d)]
                  (sort' nr (rules pat++signals pat))++
  -- Zet hierna referenties neer naar alle gebruikte en eerder gedefinieerde concepten (vueConcepts)
        [ "\t"++if length vueConcepts==1 then str4++" "++latexEmph c++" "++str5++"~\\ref{dfn:"++c++"} "++str6++"~\\pageref{dfn:"++c++"}.\n" else
          (if null newConcepts
           then str7
           else str8)++"\n\t"++
           en [latexEmph c++ " (def.~\\ref{dfn:"++c++"}, pg.~\\pageref{dfn:"++c++"})"|c<-map (unCap.name) vueConcepts]++".\n"
        | not (null vueConcepts), c<-[(unCap.name.head) vueConcepts]]++
        [ "\t"++if length newConcsNoDefs==1 then str4++" "++(latexEmph.unCap.name) c++" "++str9 else
          str10++" "++en (map (latexEmph.unCap.name) newConcsNoDefs)++" "++str11++".\n"
        | not (null newConcsNoDefs), c<-[head newConcsNoDefs]]++
        [ "\t"++str12++" "++firstCaps (name pat)++" "++str12++" \\ref{fig: concAnal"++clname (firstCaps (name pat))++"}.\n"]++
        [ latexFigureHere (latexCenter ("  \\includegraphics[scale=.4]{"++name context++"_"++clname (name pat)++".png}\n"++
                                        "  \\caption{"++str14++" "++firstCaps (name pat)++"}\n"++
                                        "  \\label{fig: concAnal"++clname (firstCaps (name pat))++"}"))]++
        patSections (prevWrittenConcepts++newConcsWithDefs) (prevWrittenDecls++newDeclarations) pats
      where
         patDecls         = decls ++ [d| d<-declarations pat, not (d `elem` decls)] -- tbv de volgorde van declaraties!
                            where decls = rd [d| r<-rules pat++signals pat, d<-declarations r]
         patConcepts      = concpts ++ [c| c<-concs pat, not (c `elem` concpts)] -- tbv de volgorde van concepten!
                            where concpts = rd [c| d<-patDecls, c<-concs d]
         vueConcepts      = [c| c<-patConcepts,      c `elem` prevWrittenConcepts ]
         newConcepts      = [c| c<-patConcepts, not (c `elem` prevWrittenConcepts)]
         newConcsWithDefs = [c| c<-newConcepts,      name c `elem` map name (conceptDefs context) ]
         newConcsNoDefs   = [c| c<-newConcepts, not (name c `elem` map name (conceptDefs context))]
         newDeclarations  = [d| d<-patDecls, not (d `elem` prevWrittenDecls)]
         explDecl :: Declaration -> [String]
         explDecl d = [ latexDesignrule (latexWord (str ++ " ("++str15++": "++name d++signature++")"))  | not (null str)]
          where
           str = explainDecl context Dutch d
           signature | length [s|s<-patDecls, name s==name d] >1  = " "++str16++" "++name (source d)++" "++str17++" "++name (target d)
                     | otherwise                                 = ""
         elaborate :: Concepts ->Declarations -> Rules -> [String]
         elaborate tobeWrittenConcs tobeWrittenDecls []
          = map (explainConcept context Dutch) tobeWrittenConcs ++
            concat (map explDecl tobeWrittenDecls)
         elaborate tobeWrittenConcs tobeWrittenDecls (r:rs)
          = map (explainConcept context Dutch) cConcs ++
            concat (map explDecl cDecls) ++
            [latexDesignrule (latexWord str)|str<-[explainRule context Dutch r], not (null str)]++
            elaborate (tobeWrittenConcs>-cConcs) (tobeWrittenDecls>-cDecls) rs
          where cConcs     = [c| c<-tobeWrittenConcs, c `elem` concs r ]
                cDecls     = [d| d<-tobeWrittenDecls, d `elem` declarations r ]

   archShow :: Lang -> Context -> String 
   archShow language context = ltshow (name context) (typology (isa context)) language context
    where
    ltshow cname typ language context
     = (chain "\n". filter (not.null))
       (if language==Dutch then 
       [ "\\title{Toetsingscriteria\\\\"++latexWord cname++"}"
       , "\\maketitle"
       , "\\tableofcontents"
       , latexChapter "Inleiding" "Inleiding"
       , "\tDit document definieert de ontwerpregels van "++name context++"."
       , "\tDeze regels moeten door de oplossing worden nageleefd."
       , "\tControle daarop vindt plaats door het architectuurteam."
       , "\tTezamen vormen deze regels de architectuur van "++name context++"."
       , designPrinciples language context
       , latexChapter "Terminologie" ("typology"++cname)
       , if null (conceptDefs context) then "" else
         latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"]
              (chain "\n" ["\\textbf{"++nm++"} & "++latexWord def++
                           (if null ref then "" else "~\\cite{"++ref++"}")++
                           "\\\\\n\\hline"
                          | Cd pos nm def ref<-conceptDefs context, cptnew nm  `elem` concs context])
       , if null cList then "" else
         if length cList==1 then "\tHet concept "++idName(head cList)++" heeft geen tekstuele definitie in sectie \\ref{typology"++cname++"}." else
         "\tDe volgende concepten zijn (nog) niet opgenomen in de woordenlijst: "++commaNL "en" (map idName (sord' name cList))++"."
       , "\\bibliographystyle{plain}"
       , "\\bibliography{../"++name context++"}"
       , "\\label{bibliography"++name context++"}"
       ] else if language==English then
       ["\\title{Functional Specification\\\\ "++latexWord cname++"}"
       , "\\maketitle"
       , "\\tableofcontents"
       , latexChapter "Introduction" "Introduction"
       , "\tThis document defines the service layer of a system called "++latexWord cname++"."
       , "\tIt defines infrastructural services in a system in which people and applications collaborate"
       , "\tto maintain agreements and commitments that apply to the context of "++latexWord cname++"."
       , "\tThese agreements and commitments are represented by rules."
       , "\tThey are presented in chapter \\ref{chp:Design rules}, arranged by theme."
       , "\tA data analysis is presented in chapter \\ref{chp:Data Analysis}."
       , "\tSubsequent chapters elaborate each theme by defining all applicable services."
       , "\tTogether, these services support all rules from chapter \\ref{chp:Design rules}."
       , "\tThis support consists of either preventing that a rule is violated,"
       , "\tsignalling violations (for human intervention),"
       , "\tor fixing the content of databases (by automatic actions) to restore a rule."
       , latexChapter "Principles" "Principles"
       , "\tThis chapter introduces guiding principles of "++latexWord cname++"."
       , "\tSubsequent chapters elaborate these principles into complete formal specifications."
       , chain "\n\n" [ latexSection ("Design choices about "++firstCaps (name pat)) ("DesignChoices"++cname++"Pat"++firstCaps (name pat)) ++
                        latexEnumerate ([latexWord (explainRule context language r)|r<-declaredRules pat++signals pat, null (cpu r)]++
                                        [latexWord (explainDecl context language d)|d<-declarations pat, (not.null) (multiplicities d)])
                      | pat<-ctxpats context]
       , latexChapter "Conceptual Analysis" "Conceptual Analysis"
       , "\tThis chapter provides an analysis of the principles described in chapter \\ref{chp:Design rules}. Each section in that chapter is analysed in terms of relations and each principle is then translated in a rule."
       , spec2fp context English spec
       , chain "\n\n" [ latexSection ("Rules about "++firstCaps (name pat)) ("Rules"++cname++"Pat"++firstCaps (name pat)) ++
  --                      "A conceptual analysis of "++firstCaps (name pat)++" is represented in figure \\ref{fig: concAnal"++clname (firstCaps (name pat))++"}.\n\n"++
  --                      latexFigureHere (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_"++clname (name pat)++".png}")++
  --                      "\n\\caption{Conceptual analysis of "++(latexWord.name) pat++"}\n\\label{fig: concAnal"++clname (firstCaps (name pat))++"}")++
                        latex "longtable" ["{|r|p{\\columnwidth}|}\\hline"]
                              (chain "\n" [show (runum r)++"&"++latexWord (explainArt context language r)++"\\\\\n&Relations:\\\\"++
                                           "&\\(\\begin{array}{rcl}\n"++
                                                chain "\\\\\n" [idName d++"&:&"++(idName.source) d++"\\times"++(idName.target) d
                                                               | d<-declarations r]++
                                                "\\end{array}\\)\\\\\n&Rule:\\\\"++
                                           "&\\(\\begin{array}{l}"++(lshow language.assemble.normRule) r++"\\end{array}\\)\\\\\\hline"|r<-declaredRules pat]
          -- als het relAlg moet zijn:     "&\\("++lshow language r                    ++"\\)\\\\\\hline"|r<-declaredRules pat]
                               )
                      | pat<-ctxpats context]
       , latexChapter "Glossary" ("typology"++cname)
       , if null (conceptDefs context) then "" else
         latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"]
               (chain "\n" ["\\textbf{"++nm++"} & "++latexWord def++"~\\cite{"++ref++"}\\\\\n\\hline"
                           | Cd pos nm def ref<-conceptDefs context, cptnew nm `elem` concs context])
       , if null cList then "" else
         if length cList==1 then "\tThe concept "++idName(head cList)++" has no textual definition in the glossary (section \\ref{typology"++cname++"})." else
         "\tThe following concepts are not described in the glossary: "++commaEng "and" (map idName (sord' name cList))++"."
       , "\\bibliographystyle{plain}"
       , "\\bibliography{../"++name context++"}"
       , "\\label{bibliography"++name context++"}"
       ] else [] )
       where
        spec = funcSpec context language
        cList = concs context>-rd [cptnew nm| Cd pos nm def ref<-conceptDefs context]
 --       nav :: Classification Concept
 --       nav  = sortCl before (Cl (Anything (genE context)) (makeTrees typ))
        mms  = declarations context
 --       degree c = length [m | m<-mms, source m==c || target m==c]
 --       c `before` c' = degree c > degree c'
 --       caps :: Classification Concept -> [String]
 --       caps (Cl c cls) = ["\\disjn{"++chain ", " (map (lshow language.root) cls)++"}"| length cls>1] ++ (concat . map capss) cls ++
 --                         concat (map caps cls)
 --       capss (Cl g cls) = ["\\super{"++lshow language g++"}{"++lshow language s++"}" | s<-map root cls]


   instance LATEX Context where
    lshow language context = ltshow (name context) (typology (isa context)) language context
    ltshow cname typ language context
     = (chain "\n". filter (not.null))
       (if language==Dutch then 
       ["\\title{Functionele Specificatie\\\\"++latexWord cname++"}"
       , "\\maketitle"
       , "\\tableofcontents"
       , latexChapter "Inleiding" "Inleiding"
       , "\tDit document definieert de servicelaag van een systeem genaamd "++latexWord cname++"."
       , "\tHet definieert infrastructuur-services in een systeem waarin mensen en applicaties samenwerken"
       , "\tom afspraken na te leven die gelden in de context van "++latexWord cname++"."
       , "\tDeze afspraken worden weergegeven door bedrijfsregels."
       , "\tDeze regels staan beschreven in hoofdstuk \\ref{chp:Ontwerpregels}, geordend op thema."
       , "\tEen gegevensanalyse volgt in hoofdstuk \\ref{chp:Gegevensanalyse}."
       , "\tIn de daarop volgende hoofdstukken is elk thema"
       , "\tuitgewerkt in definities van services."
       , "\tDeze services ondersteunen gezamenlijk alle afspraken uit hoofdstuk \\ref{chp:Ontwerpregels}."
       , "\tDeze ondersteuning bestaat uit het voorkomen dat een afspraak wordt overtreden,"
       , "\tof het signaleren van overtredingen (opdat mensen kunnen ingrijpen)"
       , "\tof het herstellen van een regel (door automatische acties op de database uit te voeren)."
       , designPrinciples language context








       , latexChapter "Conceptuele Analyse" "Conceptuele Analyse"
       , "\tDit hoofdstuk geeft een analyse van de regels uit hoofdstuk \\ref{chp:Ontwerpregels}."
       , "\tIeder thema in dat hoofdstuk wordt geanalyseerd in termen van relaties"
       , "\ten elke afspraak krijgt een formele representatie."
       , "\n\tDe resultaten van functiepunt analyse staan vermeld in tabel \\ref{tab:FPA}"
       , spec2fp context Dutch spec
       , chain "\n\n" [ latexSection ("Regels over "++latexWord (name pat)) ("Rules"++cname++"Pat"++firstCaps (name pat)) ++
  --                      "Een conceptuele analyse over "++firstCaps (name pat)++" is weergegeven in figuur \\ref{fig: concAnal"++clname (firstCaps (name pat))++"}.\n\n"++
  --                      latexFigureHere (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_"++clname (name pat)++".png}")++
  --                      "\n\\caption{Conceptuele analyse van "++(latexWord.name) pat++"}\n\\label{fig: concAnal"++clname (firstCaps (name pat))++"}\n")++
                        latex "longtable" ["{|r|p{\\columnwidth}|}\\hline"]
                              (chain "\n" [show (runum r)++"&"++latexWord (explainArt context language r)++"\\\\\n&Relaties:\\\\"++
                                           "&\\(\\begin{array}{rcl}\n"++
                                                chain "\\\\\n" [idName d++"&:&"++(idName.source) d++"\\times"++(idName.target) d
                                                               | d<-declarations r]++
                                                "\\end{array}\\)\\\\\n&Regel:\\\\"++
                                           "&\\(\\begin{array}{l}"++(lshow language.assemble.normRule) r++"\\end{array}\\)\\\\\\hline"|r<-declaredRules pat]
          -- als het relAlg moet zijn:     "&\\("++lshow language r                    ++"\\)\\\\\\hline"|r<-declaredRules pat]
                               )
                      | pat<-ctxpats context]
       , latexChapter "Gegevensanalyse" "Gegevensanalyse"
       , "\tDe keuzes, zoals beschreven in hoofdstuk \\ref{chp:Ontwerpregels} zijn in een gegevensanalyse vertaald naar"
       , "\thet klassediagram in figuur \\ref{fig:"++cname++"CD}."
       , latexFigure (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_CD.png}")++
         "\n\\caption{Gegevensmodel van "++latexWord cname++"}\n\\label{fig:"++cname++"CD}")
       , "\tDit hoofdstuk geeft een uitwerking van de gegevensanalyse in de vorm van functionele specificaties."
       , funcSpecLaTeX context (funcSpec context Dutch) Dutch
       , latexChapter "Terminologie" ("typology"++cname)
 --      , "De terminologie is ontleend aan het Divisie Informatieplan \\cite{TPDI},"
 --      , "de begrippenlijst voor \\mulF{} \\cite{MultiFit} en de begrippen gebruikt in de voorstudie SBD \\cite{SBD}."
 --      , "In geval van conflicten gaan begrippen uit \\cite{TPDI} v\\'o\\'or \\cite{MultiFit} en begrippen uit \\cite{MultiFit} v\\'o\\'or \\cite{SBD}."
       , if null (conceptDefs context) then "" else
         latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"]
              (chain "\n" ["\\textbf{"++nm++"} & "++latexWord def++
                           (if null ref then "" else "~\\cite{"++ref++"}")++
                           "\\\\\n\\hline"
                          | Cd pos nm def ref<-conceptDefs context, cptnew nm `elem` concs context])
       , if null cList then "" else
         if length cList==1 then "\tHet concept "++idName(head cList)++" heeft geen tekstuele definitie in sectie \\ref{typology"++cname++"}." else
         "\tDe volgende concepten zijn (nog) niet opgenomen in de woordenlijst: "++commaNL "en" (map idName (sord' name cList))++"."
       , "\\bibliographystyle{plain}"
       , "\\bibliography{../"++name context++"}"
       , "\\label{bibliography"++name context++"}"
       ] else if language==English then
       ["\\title{Functional Specification\\\\ "++latexWord cname++"}"
       , "\\maketitle"
       , "\\tableofcontents"
       , latexChapter "Introduction" "Introduction"
       , "\tThis document defines the service layer of a system called "++latexWord cname++"."
       , "\tIt defines infrastructural services in a system in which people and applications collaborate"
       , "\tto maintain agreements and commitments that apply to the context of "++latexWord cname++"."
       , "\tThese agreements and commitments are represented by rules."
       , "\tThey are presented in chapter \\ref{chp:Design rules}, arranged by theme."
       , "\tA data analysis is presented in chapter \\ref{chp:Data Analysis}."
       , "\tSubsequent chapters elaborate each theme by defining all applicable services."
       , "\tTogether, these services support all rules from chapter \\ref{chp:Design rules}."
       , "\tThis support consists of either preventing that a rule is violated,"
       , "\tsignalling violations (for human intervention),"
       , "\tor fixing the content of databases (by automatic actions) to restore a rule."
       , designPrinciples language context
       , latexChapter "Conceptual Analysis" "Conceptual Analysis"
       , "\tThis chapter provides an analysis of the principles described in chapter \\ref{chp:Design rules}. Each section in that chapter is analysed in terms of relations and each principle is then translated in a rule."
       , spec2fp context English spec
       , chain "\n\n" [ latexSection ("Rules about "++latexWord (name pat)) ("Rules"++cname++"Pat"++firstCaps (name pat)) ++
  --                      "A conceptual analysis of "++latexWord (name pat)++" is represented in figure \\ref{fig: concAnal"++clname (firstCaps (name pat))++"}.\n\n"++
  --                      latexFigureHere (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_"++clname (name pat)++".png}")++
  --                      "\n\\caption{Conceptual analysis of "++(latexWord.name) pat++"}\n\\label{fig: concAnal"++clname (firstCaps (name pat))++"}")++
                        latex "longtable" ["{|r|p{\\columnwidth}|}\\hline"]
                              (chain "\n" [show (runum r)++"&"++latexWord (explainArt context language r)++"\\\\\n&Relations:\\\\"++
                                           "&\\(\\begin{array}{rcl}\n"++
                                                chain "\\\\\n" [idName d++"&:&"++(idName.source) d++"\\times"++(idName.target) d
                                                               | d<-declarations r]++
                                                "\\end{array}\\)\\\\\n&Rule:\\\\"++
                                           "&\\(\\begin{array}{l}"++(lshow language.assemble.normRule) r++"\\end{array}\\)\\\\\\hline"|r<-declaredRules pat]
          -- als het relAlg moet zijn:     "&\\("++lshow language r                    ++"\\)\\\\\\hline"|r<-declaredRules pat]
                               )
                      | pat<-ctxpats context]
       , latexChapter "Data Analysis" "Data Analysis"
       , "\tA data analysis of the principles from the previous chapter (\\ref{chp:Design rules}) yields a class diagram,"
       , "\twhich shown in figure \\ref{fig:"++cname++"CD}."
       , latexFigure (latexCenter ("  \\includegraphics[scale=.4]{"++cname++"_CD.png}")++
         "\n\\caption{Data structure of "++latexWord cname++"}\n\\label{fig:"++cname++"CD}")
       , "\tDetails are provided in the following sections."
       , funcSpecLaTeX context (funcSpec context English) English
       , latexChapter "Glossary" ("typology"++cname)
       , if null (conceptDefs context) then "" else
         latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"]
               (chain "\n" ["\\textbf{"++nm++"} & "++latexWord def++"~\\cite{"++ref++"}\\\\\n\\hline"
                           | Cd pos nm def ref<-conceptDefs context, cptnew nm `elem` concs context])
       , if null cList then "" else
         if length cList==1 then "\tThe concept "++idName(head cList)++" has no textual definition in the glossary (section \\ref{typology"++cname++"})." else
         "\tThe following concepts are not described in the glossary: "++commaEng "and" (map idName (sord' name cList))++"."
       , "\\bibliographystyle{plain}"
       , "\\bibliography{../"++name context++"}"
       , "\\label{bibliography"++name context++"}"
       ] else [] )
       where
        spec = funcSpec context language
        cList = concs context>-rd [cptnew nm| Cd pos nm def ref<-conceptDefs context]
 --       nav :: Classification Concept
 --       nav  = sortCl before (Cl (Anything (genE context)) (makeTrees typ))
        mms  = declarations context
 --       degree c = length [m | m<-mms, source m==c || target m==c]
 --       c `before` c' = degree c > degree c'
 --       caps :: Classification Concept -> [String]
 --       caps (Cl c cls) = ["\\disjn{"++chain ", " (map (lshow language.root) cls)++"}"| length cls>1] ++ (concat . map capss) cls ++
 --                         concat (map caps cls)
 --       capss (Cl g cls) = ["\\super{"++lshow language g++"}{"++lshow language s++"}" | s<-map root cls]

   explainConcept :: Context -> Lang -> Concept -> String
   explainConcept thisCtx l c
    = latexDefinition (name c) [ "Een \\define{"++(unCap.name) c++"} is "++
                                 (if null expla then "nog niet gedefinieerd." else latexWord expla)++
                                 (if null ref then ".\n" else "~\\cite{"++ref++"}.")
                               | Cd pos nm expla ref<-conceptDefs thisCtx, nm==name c] ++"\n"++
      if null gens then "" else
       latexDesignrule (latexWord (upCap (plural l (name c))++" zijn "++
                                    commaNL "en" [unCap (plural l (name g))| g<-gens]++".\n"))
  {- Voorbeelden toevoegen:
      ++latexWord (if null examples then "" else
       if length examples==1 then "\nEen voorbeeld is "++"\""++head examples++"\""++"." else
       "Voorbeelden van "++upCap (plural l (name c))++" zijn "++commaNL "en" ["\""++e++"\""| e<-examples]++".")
  -}
      where gens = [g| g<-concs thisCtx, g/=c, g `gE` c, null [e| e<-(concs thisCtx>-[g,c]), g `gE` e, e `gE` c]]
                   where gE = genE c
            examples = take 3 [e| e<-conts c, fstLetter<-take 1 e, isAlpha fstLetter]
   explainRule :: Context -> Lang -> Rule -> String
   explainRule thisCtx l r
    = if null (explain r)
      then (if l==English then "Artificial explanation: " else
            if l==Dutch   then "Kunstmatige uitleg: " else
            error("Module PredLogic: unsupported language"))++(lang l .assemble.normRule) r
      else (if explain r=="NONE" then "" else explain r)
   explainDecl :: Context -> Lang -> Declaration -> String
   explainDecl thisCtx language d
    | explain d=="NONE" = ""
    | null (explain d)  = explainMult thisCtx language d
    | otherwise         = explain d
   explainMult :: Context -> Lang -> Declaration -> String
   explainMult thisCtx Dutch d
    | null ([Sym,Asy]         >- multiplicities d) = name d++" is een eigenschap van "++(unCap.plural Dutch .name.source) d++"."
    | null ([Sym,Rfx,Trn]     >- multiplicities d) = name d++" is een equivalentierelatie tussen "++(unCap.plural Dutch .name.source) d++"."
    | null ([Asy,Trn]         >- multiplicities d) = name d++" is een ordeningsrelatie tussen "++(unCap.plural Dutch .name.source) d++"."
    | null ([Sym,Asy]         >- multiplicities d) = name d++" is een eigenschap van "++(unCap.plural Dutch .name.source) d++"."
    | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d)
                                                     ++" en vice versa."
    | null ([Uni,Tot,Inj    ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d)
                                                     ++", maar niet voor elke "++(unCap.name.target) d++" hoeft er een "++(unCap.name.source) d++" te zijn."
    | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d)
                                                     ++", maar elke "++(unCap.name.target) d++" is gerelateerd aan één of meer "++(unCap.plural Dutch .name.source) d++"."
    | null ([Uni,    Inj,Sur] >- multiplicities d) = "Er is precies één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
                                                     ++applyM d "b" "a"
                                                     ++", maar niet voor elke "++(unCap.name.source) d++" hoeft er een "++(unCap.name.target) d++" te zijn."
    | null ([    Tot,Inj,Sur] >- multiplicities d) = "Er is precies één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
                                                     ++applyM d "b" "a"
                                                     ++", maar elke "++(unCap.name.source) d++" mag gerelateerd zijn aan meerdere "++(unCap.plural Dutch .name.target) d++"."
    | null ([Uni,Tot        ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d)++"."
    | null ([Uni,    Inj    ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("ten hoogste één "++(unCap.name.target) d)
                                                     ++" en elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste één "++(unCap.name.source) d++"."
    | null ([Uni,        Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("ten hoogste één "++(unCap.name.target) d)
                                                     ++", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan tenminste één "++(unCap.name.source) d++"."
    | null ([    Tot,Inj    ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("tenminste één "++(unCap.name.target) d)
                                                     ++", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste één "++(unCap.name.source) d++"."
    | null ([    Tot,    Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("tenminste één "++(unCap.name.target) d)
                                                     ++" en elke "++(unCap.name.target) d++" is gerelateerd aan tenminste één "++(unCap.name.source) d++"."
    | null ([        Inj,Sur] >- multiplicities d) = "Er is precies één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
                                                     ++applyM d "b" "a"++"."
    | null ([            Sur] >- multiplicities d) = "Er is tenminste één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
                                                     ++applyM d "b" "a"++"."
    | null ([        Inj    ] >- multiplicities d) = "Er is hooguit één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
                                                     ++applyM d "b" "a"++"."
    | null ([    Tot        ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("tenminste één "++(unCap.name.target) d)++"."
    | null ([Uni            ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("nul of één "++(unCap.name.target) d)++"."
    | otherwise                                    = applyM d ("een "++(unCap.name.source) d) ("een "++(unCap.name.target) d) ++"."
   explainMult thisCtx _ d -- default English
    | null ([Sym,Asy]         >- multiplicities d) = name d++" is a property of "++(unCap.plural English .name.source) d++"."
    | null ([Sym,Rfx,Trn]     >- multiplicities d) = name d++" is an equivalence relation on "++(unCap.plural English .name.source) d++"."
    | null ([Asy,Trn]         >- multiplicities d) = name d++" is an ordering relation on "++(unCap.plural English .name.source) d++"."
    | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
                                                     ++" and vice versa."
    | null ([Uni,Tot,Inj    ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
                                                     ++", but not for every "++(unCap.name.target) d++" there must be a "++(unCap.name.source) d++"."
    | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
                                                     ++", but every "++(unCap.name.target) d++" is related to one or more "++(unCap.plural English .name.source) d++"."
    | null ([Uni,    Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                     ++applyM d "b" "a"
                                                     ++", but not for every "++(unCap.name.source) d++" there must be a "++(unCap.name.target) d++"."
    | null ([    Tot,Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                     ++applyM d "b" "a"
                                                     ++", but every "++(unCap.name.source) d++" is related to one or more "++(unCap.plural English .name.target) d++"."
    | null ([Uni,Tot        ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)++"."
    | null ([Uni,    Inj    ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at most one "++(unCap.name.target) d)
                                                     ++" and every "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d++"."
    | null ([Uni,        Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at most one "++(unCap.name.target) d)
                                                     ++", whereas every "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d++"."
    | null ([    Tot,Inj    ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)
                                                     ++", whereas every "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d++"."
    | null ([    Tot,    Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)
                                                     ++" and every "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d++"."
    | null ([        Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                     ++applyM d "b" "a"++"."
    | null ([            Sur] >- multiplicities d) = "There is at least one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                     ++applyM d "b" "a"++"."
    | null ([        Inj    ] >- multiplicities d) = "There is at most one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                     ++applyM d "b" "a"++"."
    | null ([    Tot        ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)++"."
    | null ([Uni            ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("zero or one "++(unCap.name.target) d)++"."
    | otherwise                                    = applyM d ("a "++(unCap.name.source) d) ("a "++(unCap.name.target) d) ++"."

   lglos language context = ltglos (name context) (typology (isa context)) language context
   ltglos cname typ language context
     = (chain "\n". filter (not.null))
       (if language==Dutch then 
       [ "\\newcommand{\\mulF}{MultiF{\\it it}}"
       , "\\title{Woordenlijst voor "++latexWord cname++"}"
       , "\\maketitle"
       , if null (conceptDefs context) then "" else
         latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"]
              (chain "\n" ["\\bf "++nm++" & "++latexWord def++
                           (if null ref then "" else "~\\cite{"++ref++"}")++
                           "\\\\\n\\hline"
                          | Cd pos nm def ref<-conceptDefs context])
       , if null cList then "" else
         if length cList==1 then "\tHet concept "++idName(head cList)++" heeft geen tekstuele definitie in sectie \\ref{typology"++cname++"}." else
         "\tDe volgende concepten zijn niet opgenomen in de woordenlijst: "++commaNL "en" (map idName (sord' name cList))++"."
       , "\\bibliographystyle{plain}"
       , "\\bibliography{../"++name context++"}"
       , "\\label{bibliography"++name context++"}"
       ] else if language==English then
       [ "\\title{Design of "++latexWord cname++"}"
       , "\\maketitle"
       , latexChapter "Glossary" ("typology"++cname)
       , if null (conceptDefs context) then "" else
         latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"] (chain "\n" ["\\bf "++nm++" & "++latexWord def++"~\\cite{"++ref++"}\\\\\n\\hline"
                           | Cd pos nm def ref<-conceptDefs context])
       , if null cList then "" else
         if length cList==1 then "\tThe concept "++idName(head cList)++" has no textual definition in the glossary (section \\ref{typology"++cname++"})." else
         "\tThe following concepts are not described in the glossary: "++commaEng "and" (map idName (sord' name cList))++"."
       , "\\bibliographystyle{plain}"
       , "\\bibliography{../"++name context++"}"
       , "\\label{bibliography"++name context++"}"
       ] else [] )
       where
        cList = concs context>-rd [cptnew (name cd)| cd<-conceptDefs context]
 --       nav :: Classification Concept
 --       nav  = sortCl before (Cl (Anything (genE context)) (makeTrees typ))
        mms  = declarations context
 --       degree c = length [m | m<-mms, source m==c || target m==c]
 --       c `before` c' = degree c > degree c'
 --       caps :: Classification Concept -> [String]
 --       caps (Cl c cls) = ["\\disjn{"++chain ", " (map (lshow language.root) cls)++"}"| length cls>1] ++ (concat . map capss) cls ++
 --                         concat (map caps cls)
 --       capss (Cl g cls) = ["\\super{"++lshow language g++"}{"++lshow language s++"}" | s<-map root cls]


   instance Identified (Typology a) where
    name typ = ""
    typ = error "No type defined for (Typology a)"
    
  -- lname and clname clean strings
   lname :: Identified a => a -> String
   lname  = clname . name
   clname :: String -> String
   clname str = [if isAlphaNum c then c else if c `elem` "/\\" then c else '_'| c<- str]

   lIntro :: Lang -> String
   lIntro language
     = chain "\n"
         [ "\\documentclass[10pt,a4paper]{report}"
         , if language==Dutch then "\\usepackage[dutch]{babel}" else ""
         , "\\parskip 10pt plus 2.5pt minus 4pt  % Extra vertical space between paragraphs."
         , "\\parindent 0em                      % Width of paragraph indentation."
         , "\\usepackage{theorem}"
         , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{definition}{"++(if language==Dutch then "Definitie" else "Definition")++"}[section]"
         , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{designrule}[definition]{"++(if language==Dutch then "Ontwerpregel" else "Design Rule")++"}"
         , "\\usepackage{graphicx}"
         , "\\usepackage{amssymb}"
         , "\\usepackage{amsmath}"
 --        , "\\usepackage{zed-csp}"
         , "\\usepackage{longtable}"
         , "\\def\\id#1{\\mbox{\\em #1\\/}}"
         , "\\def\\define#1{\\label{dfn:#1}{\\em #1}}"












         , "\\newcommand{\\iden}{\\mathbb{I}}"
         , "\\newcommand{\\ident}[1]{\\mathbb{I}_{#1}}"
         , "\\newcommand{\\full}{\\mathbb{V}}"
         , "\\newcommand{\\fullt}[1]{\\mathbb{V}_{[#1]}}"
         , "\\newcommand{\\relAdd}{\\dagger}"
         , "\\newcommand{\\flip}[1]{{#1}^\\smallsmile} %formerly:  {#1}^\\backsim"
         , "\\newcommand{\\kleeneplus}[1]{{#1}^{+}}"
         , "\\newcommand{\\kleenestar}[1]{{#1}^{*}}"
         , "\\newcommand{\\cmpl}[1]{\\overline{#1}}"
         , "\\newcommand{\\rel}{\\times}"
         , "\\newcommand{\\compose}{;}"
         , "\\newcommand{\\subs}{\\vdash}"
         , "\\newcommand{\\fun}{\\rightarrow}"
         , "\\newcommand{\\isa}{\\sqsubseteq}"
         , "\\newcommand{\\N}{\\mbox{\\msb N}}"
         , "\\newcommand{\\disjn}[1]{\\id{disjoint}(#1)}"
         , "\\newcommand{\\fsignat}[3]{\\id{#1}:\\id{#2}\\mbox{$\\rightarrow$}\\id{#3}}"
         , "\\newcommand{\\signat}[3]{\\mbox{${#1}_{[{#2},{#3}]}$}}"
         , "\\newcommand{\\declare}[3]{\\id{#1}:\\id{#2}\\mbox{$\\times$}\\id{#3}}"
         , "\\newcommand{\\fdeclare}[3]{\\id{#1}:\\id{#2}\\mbox{$\\fun$}\\id{#3}}"]

   generateGlossaryLaTeX :: Context -> Lang -> String
   generateGlossaryLaTeX context language
     = lIntro language++concat
         [ "\n\\begin{document}"
         , if language==English then "\n" else "\n\\selectlanguage{dutch}\n"
         , chain "\n" [lglos language c| c<-(rd' name . preCl . Cl context . ctxwrld) context]
         , "\n\\end{document}"
         ]


   generateFspecLaTeX :: Context -> Lang -> [Ftheme] -> String
   generateFspecLaTeX context language spec
     = lIntro language++concat
         [ "\n\\begin{document}"
         , if language==English then "\n" else "\n\\selectlanguage{dutch}\n"
         , chain "\n" [lshow language c| c<-(rd' name . preCl . Cl context . ctxwrld) context]
         , "\n\\end{document}"
         ]

   generateArchLaTeX :: Context -> Lang -> [Ftheme] -> String
   generateArchLaTeX context language spec
     = lIntro language++concat
         [ "\n\\begin{document}"
         , if language==English then "\n" else "\n\\selectlanguage{dutch}\n"
         , chain "\n" [archShow language c| c<-(rd' name . preCl . Cl context . ctxwrld) context]
         , "\n\\end{document}"
         ]


















   latexSchema :: String -> [String] -> String
   latexSchema nm body = latex "schema" ["{"++nm++"}"] (chain "\\\\\n" body)

   lschema cname (Typ pths) language pat
    = if null (ptrls pat) then "void" else
      (latexSchema (ptnm pat) . filter (not.null))
      [ {- "  "++cname++" Concepts"
      , "\\where"
      , -} chain "\n" ["  "++(ltshow cname (Typ pths) language.assemble.normRule) r++"\\\\" | r <- declaredRules pat]
      , chain "\n" ["  "++(ltshow cname (Typ pths) language.assemble.normRule) s++"\\\\" | s <- specs pat]
      ] ++ "\n" ++
      (chain "\n\n" . map (latexWord.explain) . declaredRules) pat
      where
       parents c = f [cptnew (name p) | [p,s]<-pths, name s==name c]
       f []  = "Anything"
       f [c] = concat [if c==' ' then "\\ " else [c]| c<-name c]
       f cs  = "("++chain "\\cup" [concat [if c==' ' then "\\ " else [c]| c<-name c] | c<-cs]++")"


   contDef :: String -> Typology Concept -> Lang -> Pattern -> String
   contDef cname typ language pat
    = lschema cname
              typ language
              (Pat (ptnm pat++" "++show (length (declaredRules pat)+1))
                   [Ru Equivalence  (F [Tm m]) pos expr cpu "" sgn nr pn
                   | Gc pos m expr cpu sgn nr pn<-specs pat] [] [] [] [])

   instance LATEX Prop where
    lshow language Uni = if language==Dutch then "univalent"    else "univalent"
    lshow language Inj = if language==Dutch then "injectief"    else "injective"
    lshow language Sur = if language==Dutch then "surjectief"   else "surjective"
    lshow language Tot = if language==Dutch then "totaal"       else "total"
    lshow language Sym = if language==Dutch then "symmetrisch"  else "symmetric"
    lshow language Asy = if language==Dutch then "antisymmetrisch" else "antisymmetric"
    lshow language Trn = if language==Dutch then "transitief"   else "transitive"
    lshow language Rfx = if language==Dutch then "reflexief"    else "reflexive"   

   instance (Identified a,LATEX a) => LATEX [a] where
    lshow language xs = chain "\n\n" [lshow language x| x<-xs]

   idName :: Identified a => a -> String
   idName c = idNam (name c)
   idNam :: String -> String
   idNam c = "\\id{"++concat [if c `elem` [' ','_'] then "\\ " else [c]| c<-firstCaps c]++"}"

   instance LATEX Declaration where
    lshow language mm@(Sgn _ _ _ _ _ _ _ _ _ _ _ _)
     = if language==Dutch then
        "\\label{rel:"++firstCaps (lname mm++lname (source mm)++lname (target mm))++"}\n"++
               wrapMath(idName mm++":: "++ lsign mm)++"\\\\\n"++
               (if null ps then "" else
                if null ([Uni,Tot]>-ps) -- zo ja dan is het een functie
                 then if null pfs
                      then idName mm++" is een functie. "
                      else "Deze functie is "++commaNL "en" [lshow language p| p<-pfs]++".\\\\\n"
                 else "Deze relatie is " ++ commaNL "en" [lshow language p| p<-ps]++".\\\\\n") ++
               (if null cs
                then "Betekenis: "++applyMLatex mm ("\\langle"++lshow language(source mm)++"\\rangle") ("\\langle"++lshow language(target mm)++"\\rangle")++"."
                else "Als bijvoorbeeld (`"++head(head cs)++"', `"++last(head cs)++"') in de relatie "++idName mm++" voorkomt, dan betekent dit: "++applyMLatex mm ("`"++head(head cs)++"'") ("`"++last(head cs)++"'"))++"."
       else if language==English then
        "\\label{rel:"++firstCaps (lname mm++lname (source mm)++lname (target mm))++"}\n"++
               wrapMath(idName mm++":: "++ lsign mm)++"\\\\\n"++
               (if null ps then "" else
                if null ([Uni,Tot]>-ps) -- zo ja dan is het een functie
                 then if null pfs
                      then idName mm++" is a function. "
                      else "This function is "++commaEng "and" [lshow language p| p<-pfs]++".\\\\\n"
                 else "This relation is " ++ commaEng "and" [lshow language p| p<-ps]++".\\\\\n") ++
               (if null cs
                then "Meaning: "++applyMLatex mm ("\\langle"++lshow language(source mm)++"\\rangle") ("\\langle"++lshow language(target mm)++"\\rangle")++"."
                else "If, for example (`"++head(head cs)++"', `"++last(head cs)++"') occurs in relation "++idName mm++", this means: "++applyMLatex mm ("`"++head(head cs)++"'") ("`"++last(head cs)++"'"))++"."
       else ""
       where cs=contents mm
             ps=multiplicities mm; pfs = ps>-[Uni,Tot]
    lshow language mm@(Isn _ _) = ""

   instance LATEX Concept where
    lshow language c = idName c

   instance LATEX Morphism where
    lshow language m
            | isIdent m = "\\ident{"++name (source m)++"}"
            | isNot m   = "\\cmpl{\\ident{"++name (source m)++"}}"
            | otherwise = if inline m then idName m else "\\flip{"++idName m++"}"

   lsign (Sgn nm d c ps _ _ _ _ _ _ _ _)
                       | m Uni&& m Inj && m Sur && m Inj = a++"\\rel"++b
                       | m Uni&& m Tot                   = a++"\\rightarrow"++b
                       | otherwise                       = a++"\\rel"++b
         where m e = e `elem` ps; a=idName d; b=idName c

 {- if used in Zed context, more specific arrows are:
                       | not (m Uni)         = a++"\\rel"++b
                       | m Inj&&m Tot&&m Sur = a++"\\bij"++b
                       | m Inj&&m Tot        = a++"\\inj"++b
                       | m Inj               = a++"\\pinj"++b
                       |        m Tot&&m Sur = a++"\\surj"++b
                       |        m Tot        = a++"\\rightarrow"++b
                       |               m Sur = a++"\\psurj"++b
                       | otherwise           = a++"\\pfun"++b
         where m e = e `elem` ps; a=idName d; b=idName c -}




   instance (Eq a, Show a, Identified a) => LATEX (Classification a) where
    lshow language cl
     = recur cl
       where
        recur (Cl r cls)
         = name r++(latexDotted . map recur . sort' name) cls





   instance LATEX Expression where
    lshow language (Tm m)   = lshow language m
    lshow language (F ts)   = chain "\\compose" (map (lshow language) ts)
    lshow language (Fd ts)  = chain "\\relAdd" (map (lshow language) ts)
    lshow language (Fu fs)  = chain "\\cup" (map (lshow language) fs)
    lshow language (Fi fs)  = chain "\\cap" (map (lshow language) fs)
    lshow language (Cp e)   = "\\cmpl{"++lshow language e++"}"
    lshow language (K0 e)   = "\\kleenestar{"++lshow language e++"}"
    lshow language (K1 e)   = "\\kleeneplus{"++lshow language e++"}"

   instance LATEX Rule where
    ltshow cname typ language (Gc pos m expr cpu sgn nr pn) = ltshow cname typ language (Ru Equivalence (F [Tm m]) pos expr cpu "" sgn nr pn)
    ltshow cname typ language r = (ltshow cname typ language.assemble.normRule) r
    lshow language r | ruleType r==Implication && fEmpty (antecedent r) = lshow language (consequent r)
                     | ruleType r==Implication && fEmpty (consequent r) = lshow language (Cp (antecedent r))
                     | ruleType r==Implication                          = lshow language (antecedent r)++"\\subs"++lshow language (consequent r)
                     | ruleType r==Equivalence = lshow language (antecedent r)++"="++lshow language (consequent r)
                     | ruleType r==AlwaysExpr  = lshow language (consequent r)
                     | otherwise               = lshow language (antecedent r)++"="++lshow language (consequent r)





   instance LATEX PredLogic where
    lshow language x = predLshow ("\\forall ", "\\exists ", implies, "\\Leftrightarrow", "=", "\\not =", "\\vee", "\\wedge", "\\neg", rel, fun, mathVars, "\\\\\n  ", "\\ ") x
                       where rel m lhs rhs = lhs++"\\ "++(if isIdent m then "=" else idName m)++"\\ "++rhs
                             fun m x = idName m++"("++x++")"
                             implies antc cons = antc++"\\ \\Rightarrow\\ "++cons

   uName :: [String] -> String -> String
   uName nms n = concat [v| (nm,v)<-zip nms' vs, n==nm]
    where nms' = rd nms
          vs = f (map (map toLower.take 1) nms')
          f (v:vs)          = if v `elem` vs then f (g v (map show [1..]) (v:vs)) else v: f vs
          f []              = []
          g e (i:is) (v:vs) = if v==e then (v++i): g e is vs else v: g e (i:is) vs
          g e _ []          = []





   projectClassic :: Context -> [Ftheme] -> Lang -> String
   projectClassic context fs language
    = "ID;Task_Name;Duration;Type;Outline_Level;Predecessors;Milestone;Rollup\n"++
      (task2proj.spec2task context English) fs

   data Task = Tsks String   -- name of group of tasks
                    String   -- affix
                    [String] -- dependencies (refer by name)
                    [Task]
             | Tsk  String   -- name of task
                    String   -- duration
                    String   -- affix
                    [String] -- dependencies (refer by name)
             | Mils String   -- name of milestone
                    [String] -- dependencies (refer by name)

   render tasks
    = [ chain ";" [id nm,nm,dur,typ,outl,dependencies deps affix,ms,rup]
      | (nm,dur,typ,outl,affix,ms,rup,deps)<-tasks]
      where id t = if null result then error ("No "++t++" known in task list") else
                   if length result>1 then error ("multiple tasks named "++t++" in task list")else
                   head result
                   where result = [show i| (i,(nm,dur,typ,outl,affix,ms,rup,deps))<-zip [1..] tasks,t==nm]
            dependencies deps affix
             = (if length deps>1 then "\""++chain ";" (map id deps)++"\"" else concat (map id deps))++affix

   type NSpace = [(Int,String)]
   task2proj t = chain "\n" result
    where
     result = task 0 1 (milSpace 1 t) t

     task :: Int->Int->NSpace->Task->[String]
     task indent i namespace (Tsk taskname dur affix deps)
      = [ chain ";" [show i,taskname,dur,"Fixed Units",show indent,render namespace deps++affix,"No","No"] ]
     task indent i namespace (Mils taskname deps)
      = [ chain ";" [show i,taskname,"0 days","Fixed Units",show indent,render namespace deps,"Yes","No"] ]
     task indent i namespace t@(Tsks taskname affix deps ts)
      = [ chain ";" [show i,taskname,"","Fixed Duration",show indent,render namespace deps++affix,"No","Yes"] ]++
        tsk (nms (i+1) ts++namespace) (i+1) ts
        where tsk namespace i (t:ts) = task (indent+1) i namespace t ++ tsk namespace (i+count t) ts
              tsk namespace i [] = []
     nm (Tsk taskname dur affix deps) = taskname
     nm (Mils taskname deps)          = taskname
     nm (Tsks taskname affix deps ts) = taskname
     count (Tsk taskname dur affix deps) = 1
     count (Mils taskname deps)          = 1
     count (Tsks taskname affix deps ts) = 1+sum (map count ts)
     nms :: Int->[Task]->NSpace
     nms i []     = []
     nms i (t:ts) = (i,nm t): nms (i+count t) ts
     milSpace i (Tsk taskname dur affix deps) = []
     milSpace i (Mils taskname deps)          = [(i,taskname)]
     milSpace i (Tsks taskname affix deps (t:ts)) = milSpace i t++milSpace (i+count t) (Tsks taskname affix deps ts)
     milSpace i (Tsks taskname affix deps []) = []
     render :: NSpace->[String]->String
     render namespace deps
      = if length deps>1 then "\""++chain ";" (map lookup deps)++"\"" else concat (map lookup deps)
        where lookup nm
               = head ([show i| (i,n)<-namespace,n==nm]++error ("Name \""++nm++"\" not found in function task2proj."))
 
   showOO = objOrShow.assemble.normRule
 
   spec2fp context lang fspcs
    = "\n\\begin{table}[htb]\\begin{center}\n"++
      "\\begin{tabular}{|l|r|}\\hline &"++str2++"\\\\\\hline\n  "++   -- str2 = "Function Point Analysis"
      (chain "\\\\\n" . map (\cl -> fst (head cl)++"&"++show (sum (map snd cl))) . eqCl fst) us++
      "\\\\\\hline\n\\end{tabular}\n"++
      "\\caption{"++str1++"}\\label{tab:FPA}"++
      "\n\\end{center}\\end{table}\n"++
      "\n\t"++str0++" \\ref{tab:FPA}."++   -- str0 = "The results of a global function point analysis conformant to IFPUG principles are given in table"
      "\n\t"++str3++" "++(show.sum.map snd) us++" "++str2++"."
      where
       us= [ (unm, fps)
           | Tspc pat units<-fspcs, Uspc unm pat car specs<-units
           , fps<-[sum ([fPoints fpa| Sspc nm sees changes fpa input output rs pre post<-specs]++
                        [fPoints fpa| (_,fpa,_,_)<-car])
                  ], fps>0
           ]
       str0 | lang==English = "The results of a global function point analysis conformant to IFPUG principles are given in table"
            | lang==Dutch   = "De resultaten van een globale functiepunt analyse conform de richtlijnen van IFPUG zijn weergegeven in tabel"
       str1 | lang==English = "Function Point Analysis"
            | lang==Dutch   = "Functiepunt Analyse"
       str2 | lang==English = "function points"
            | lang==Dutch   = "functiepunten"
       str3 | lang==English = "This yields a total of"
            | lang==Dutch   = "Het totaal is"
   spec2task context English fspcs
    = Tsks "Project" "" [] [design fspcs,tools fspcs,build fspcs,implement fspcs]
      where
       design fs
        = Tsks "Design phase" "" []
          [ Tsk "select design & build teams" "1 mon" "" []
          , Tsk "Requirements elicitation" "2 mons" "" ["select design & build teams"]
          , Tsk "Approve requirements" "1 mon" "" ["Requirements elicitation"]
          , Tsk "Definition study" "2 mons" "" ["Requirements elicitation"]
          , Mils "Concept" ["Definition study"]
          , Tsk "Approve concept" "1 mon" "" ["Concept"]
          , Tsk "Process modeling" "4 mons" "" ["Concept"]
          , Tsk "Data definition" "2 mons" "" ["Concept"]
          , Tsk "Approve data definition" "1 mon" "" ["Data definition"]
          , Tsk "Functional Specification" "4 mons" "" ["Data definition"]
          , Mils "Functional Spec" ["Functional Specification"]
          , Tsk "Approve func. spec." "1 mon" "" ["Functional Spec"]
          ]
       tools fs
        = Tsks "Tools phase" "" ["Concept"]
          [ Tsk "Selection" "1 mon" "" []
          , Tsk "Tendering & acquistion" "3 mons" "" ["Selection"]
          , Tsk "installation" "1 mon" "" ["Tendering & acquistion"]
          ]
       build fs
        = Tsks "Building phase" "" ["Design phase","Tools phase"]
          [ Tsks "process logic" "" []
                [ Tsk "build" "3 mons" "" []
                , Tsk "unit test" "2 mons" "FF+1 mon" ["build"]
                ]
          , Tsks "user interfaces" "" []
                [ Tsk "build" "9 mons" "" []
                , Tsk "unit test" "8 mons" "FF+1 mon" ["build"]
                ]
          , Tsks "service layer & database" "" ["Functional Spec"]
                [ Tsk "build database" "2 mons" "" []
                , Tsk "build services" "11 mons" "SS+1 mon" ["build database"]
                , Tsk "unit test" "7 mons" "FF+1 mon" ["build services"]
                ]
          , Tsk "Integration test" "3 mons" "" ["process logic","user interfaces","service layer & database"]
          , Tsk "Acceptance test" "1 mon" "" ["Integration test"]
          ]
       implement fs
        = Tsks "Implementation phase" "" []
          [ Tsk "Preparation" "6 mons" "FF" ["Building phase"]
          , Tsk "Roll-out" "2 mons" "" ["Preparation"]
          , Tsk "user training" "2 mons" "FF-2 wks" ["Roll-out"]
          , Tsk "Transfer" "1 mon" "" ["Roll-out"]
          , Tsk "after care" "3 mons" "" ["Transfer"]
          ]

