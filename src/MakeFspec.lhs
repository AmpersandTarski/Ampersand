
> module MakeFspec (makeFspecNew2,Fspc)
> where

  This module is for the purpose of translating a Concept into a Fspec

>  import CommonClasses ( Identified(name))
>  import Collection    ( Collection (isc,(>-),rd) )
>  import Auxiliaries(sort,sort',snd3,eqCl,fst3,eqClass,chain)
>  import Char(toLower)
>  import Strings(firstCaps,tt,idNam)
>  import ADLdef
>  import CC_aux (mIs,showADL)
>  import FspecDef

>  import Calc(lClause,rClause,makeRule)
>  import ERmodel(erAnalysis)

>-- The story:
>-- A number of datasets for this context is identified.
>-- Every pattern is considered to be a theme and every object is treated as a separate object specification.
>-- Every dataset is discussed in precisely one theme
>-- Every theme will be explained in a chapter of its own.

>  makeFspecNew2 :: Context -> Fspc
>  makeFspecNew2 context
>    = Fspc fid themes datasets fviews frules where
>       fid      = makeFSid1 (name context)
>       themes   = (  [makeFtheme context pat ds| (pat,ds)<-pats]                      -- one pattern yields one theme
>                  ++ [makeFtheme context others remainingDS| not (null remainingDS)]  -- remaining datasets are discussed at the end
>                  )
>       datasets = rd [datasetMor context m| m<-mors context]
>       fviews   = [ makeFview context a | a <-attributes context]
>       frules   = [ makeFrule context r | r <-rules context]


>-- next thing, we look which datasets will be discussed in which themes.
>-- Priority is given to those patterns that contain a concept definition of a root concept of the dataset,
>-- because we assume that the programmer found it important enough to define that concept in that pattern.
>-- in order to ensure that at most one pattern discusses a dataset, double (pat,cs,d)-triples are dropped.
>       pcsds0 = (map (head.sort' snd3).eqCl (name.fst3))
>                [ (pat,length cns,ds)
>                | pat<-patterns context, ds<-datasets, cns<-map name (concs ds) `isc` [name c|c<-conceptDefs pat], not (null cns)]
>-- Now, pcsds0 covers concepts that are both root of a dataset and are defined in a pattern.
>-- The remaining concepts and datasets are determined in pcsds1.
>-- A dataset is assigned to the pattern with the most morphisms about the root(s) of the dataset.
>       pcsds1 = (map (head.sort' snd3).eqCl (name.fst3))
>                [ (pat,0-length ms,ds)
>                | pat<-patterns context, ds <- datasets>-[ds|(_,_,ds)<-pcsds0]
>                , ms<-[[m|m<-morlist pat, m `elem` mors ds || flp m `elem` mors ds]], not (null ms)
>                ]
>-- The remaining datasets will be discussed in the last theme
>       remainingDS = datasets>-[ds'|(_,_,ds')<-pcsds0++pcsds1]
>       others
>        = Pat "Other topics" rs gen pms cs ks
>          where rs  = []
>                gen = []
>                pms = rd [d| ds<-remainingDS, d<-declarations ds]
>                cs  = []
>                ks  = []
>   --    context' = Ctx nm on i world pats rs ds cs ks os pops
>   --       where nm    = name context
>   --             on    = extends context
>   --             i     = isa context
>   --             world = wrld context
>   --             pats  = patterns context ++ if null remainingDS then [] else [others]
>   --             rs    = rules context
>   --             ds    = declarations context
>   --             cs    = conceptDefs context
>   --             ks    = []
>   --             os    = attributes context
>   --             pops  = populations context
>-- The patterns with the appropriate datasets are determined:
>       pats = [ (pat, [dg| (p,_,dg)<-pcsds0++pcsds1, name pat==name p]) | pat<-patterns context]


Precondition: the list of datasets must contains functionally equivalent datasets.
This means that if d,e are datasets in dgs, then there is a bijective function between root d and root e in mors pat.
Motivation: we want to make one textual unit per dataset, but equivalent datasets are discussed in the same unit.

>  makeFtheme :: Context -> Pattern -> [Dataset] -> Ftheme
>  makeFtheme context pat dss
>   = Tspc fid units 
>     where
>       fid = makeFSid1 (name pat)
>       units = [makeFunit context pat (objs ds) [] []| ds<-dss]
>        where
>         objs ds = [o| o<-attributes context, makeDataset context (concept o)==ds]

>  datasetMor :: Context -> Morphism -> Dataset
>  datasetMor context m | isFunction      m  = makeDataset context (source m)
>                       | isFunction (flp m) = makeDataset context (target m)
>                       | otherwise          = BR m

>  makeFview :: Context -> ObjectDef -> Fview
>  makeFview context o
>   = Fview (makeDataset context (concept o)) o
>          ([ getEach context o
>           , createObj context o [] {-rs-}
>           , readObj context o
>           , deleteObj context o [] {-rs-}
>           , updateObj context o [] {-cs-} [] {-rs-} ])
>          [makeFrule context r| r<-rules context, not (null (mors r `isc` mors o))]  -- include all valid rules that relate directly to o.

>  makeFrule :: Context -> Rule -> Frule
>  makeFrule context r = Frul r

>  makeFunit :: Context -> Pattern -> [ObjectDef] -> [Concept] -> [ServiceSpec] -> Funit
>  makeFunit context pat objs newConcs newDecls
>   = Uspc fid pat ents svs
>       where
>         fid  = (if null objs then NoName else makeFSid1(name (head objs))) 
>         ents = [(o
>               -- ,ILGV Eenvoudig
>                  ,[] {-cs-}
>                  ,[] {-rs-}
>                 )| o<-objs]
>         svs  = (concat [   [ createObj context o [] {-rs-} ]
>                         ++ [ readObj context o]
>                         ++ concat [ [keyEnt context o (key,ks), delKeyEnt context o (key,ks) [] {-rs-}]
>                                     | (e,key,ks)<-keys pat, e==concept o]
>                         ++ [ deleteObj context o [] {-rs-} ]
>                         ++ [ updateObj context o [] {-cs-} [] {-rs-}| not (null [] {-cs-}) ]
>                    | o<-objs ])

Any concept that is a specialisation of a generic concept will be represented in the table of the generic concept.
De function dataset computes in which dataset a concept is represented.

>  makeDataset :: Context -> Concept -> Dataset
>  makeDataset context c
>   = DS (minimum [g|g<-concs context,g<=head cl]) dss
>     where
>      cl   = head ([cl| cl<-eCls, c `elem` cl]++error ("!Fatal (module Fspec>dataset): cannot determine dataset for concept "++name c))
>      eCls = eqClass bi (concs context)
>      c `bi` c' = not (null [m| m<-declarations context, isFunction m, isFunction (flp m)
>                              , source m<=c && target m<=c'  ||  source m<=c' && target m<=c])
>      dss = [     makeMph d | d<-declarations context, isFunction      d , source d `elem` cl]++
>            [flp (makeMph d)| d<-declarations context, isFunction (flp d), target d `elem` cl]



De volgende functie vult een procedure in voor een bepaalde rol.
Aanpak:
1. Alle relaties, die in de regels van deze service genoemd worden, zijn zichtbaar voor deze rol.
2. Een conjunct kan in stand gehouden worden als één van de termen ervan door deze rol geedit kan worden.
3. Alle conjuncts uit de service moeten door deze rol in deze service in stand gehouden worden.
4. Als een conjunct automatisch in stand gehouden wordt, is er geen eis aan de service
5. Als een conjunct handmatig in stand gehouden wordt:
5.a. moet overtreding ervan aan deze rol worden gesignaleerd;
5.b. moet één van de termen van deze conjunct editbaar zijn door deze rol.
Te bepalen:
 - welke velden zijn zichtbaar?
 - welke velden zijn editbaar?
 - welke signalen zijn zichtbaar?

>  getEach :: Context -> ObjectDef -> ServiceSpec 
>  getEach context o
>   = Sspc fid maySee mayChange
>          -- No more FPA here
>          params results invariants preconds postconds 
>       where
>         fid         = makeFSid1 ("getEach_"++name o)
>         maySee      = [mIs (concept o)]  -- see  concept o  only.
>         mayChange   = []                 -- change nothing
>         params      = []                 -- input parameters
>         results     = [ Aspc (makeFSid1 "objs") ("["++handle context o++"]")]  -- results
>         invariants  = []                 -- rules
>         preconds    = []                 -- Precondition
>         postconds   = ([tt "objs"++"= I["++name (concept o)++"]"]) -- Postcondition

>  createObj :: Context -> ObjectDef -> [(Expression,Rule)] -> ServiceSpec 
>  createObj context o rs
>   = Sspc fid maySee mayChange
>          -- No more FPA here
>          params results invariants preconds postconds 
>       where
>         fid         = makeFSid1 ("create_"++name o)
>         maySee      = ([mIs (concept o)]++mors o)      -- see the morphisms touched by this object
>         mayChange   = (mors o)                         -- change these morphisms
>     --     (IF Gemiddeld)
>         params      = [ Aspc (makeFSid1(varName (name a))) (handle context a) | a<-attributes o]  -- input parameters
>         results     = [ Aspc (makeFSid1("obj")) (handle context o)] -- results
>         invariants  = (dressRules
>                        [ (clause,rule)
>                        | (conj,rule)<-rs
>                        , clause@(Fu terms)<-[lClause conj]
>                        , not (null (mors o `isc` mors [t| Cp t<-terms]))])
>         preconds    =  []
>--    Post (example:) {o in Pair and o left l and o right r}
>         postconds   =  ([tt ("obj."++name a)++"="++tt (varName (name a))|a<-attributes o])
>         varName :: String -> String
>         varName = uName (map name (attributes o))
>  readObj :: Context -> ObjectDef -> ServiceSpec
>  readObj context o
>   = Sspc fid maySee mayChange
>          -- No more FPA here
>          params results invariants preconds postconds 
>       where
>         fid         = makeFSid1 ("read_"++name o)
>         maySee      = ([mIs (concept o)]++mors o) -- sees
>         mayChange   = []                          -- change nothing
>      --    (OF Eenvoudig)
>         params      = [Aspc (makeFSid1 "x") (handle context o)]
>         results     = [Aspc (makeFSid1 (varName (name a))) (handle context a) | a<-attributes o]
>         invariants  = []
>--    Pre (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
>         preconds    = ([ tt ("x."++name a)++"="++idNam (nameAt a) |a<-attributes o])
>--    Post (example:) {left=l, right=r, src=s, and trg=t}
>         postconds   = [tt (varName (name a))++"="++idNam (nameAt a)|a<-attributes o]
>         varName :: String -> String
>         varName = uName (map name (attributes o))

>  keyEnt :: Context -> ObjectDef -> (String,[ObjectDef]) -> ServiceSpec
>  keyEnt context o (key,ats')
>   = Sspc fid maySee mayChange
>          -- No more FPA here
>          params results invariants preconds postconds 
>       where
>         fid         = makeFSid1 ("sel_"++name o++"_by_"++if null key then chain "_" (map name ats') else key)
>    --     serviceName = (firstCaps ("sel"++name o++"_by_"++if null key then chain "_" (map name ats') else key))
>         maySee      = (mors context) -- see everything
>         mayChange   = []             -- change nothing
>     --     (OF Eenvoudig)
>         params      = [ Aspc (makeFSid1(varName (name a))) (handle context a) | a<-ats']
>         results     = [Aspc (makeFSid1 "obj") (handle context o)]
>         invariants  = []
>--    Pre (example:) {Assume l=atom_left and r=atom_right}
>         preconds    = [let args = [tt ("x."++name a)++"="++tt (varName (name a)) | a<-attributes o] ++
>                                   [tt ("x."++name a)++"="++idNam (nameAt a)      | a<-attributes o, not (name a `elem` map name ats')]
>                        in
>                        "\\hbox{There is an {\\tt x}}\\in"++idName (concept o)++"\\ \\hbox{such that}"++
>                        (if length args==1 then "\\ "++concat args else
>                         "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
>                         chain "\\\\\n" (map ('&':) args)++
>                         "&)\n\\end{array}$"
>                        )]
>--    Post (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
>         postconds   = ([ tt "obj"++"="++tt "x"])
>         varName :: String -> String
>         varName = uName (map name (attributes o))

>  delKeyEnt :: Context -> ObjectDef -> (String,[ObjectDef]) -> [(Expression,Rule)] -> ServiceSpec
>  delKeyEnt context o (key,ats') rs
>   = Sspc fid maySee mayChange
>          -- No more FPA here
>          params results invariants preconds postconds 
>       where
>         fid         = makeFSid1 ("del_"++name o++"_by_"++if null key then chain "_" (map name ats') else key)
>      --   serviceName = (firstCaps ("del"++name o++"_by_"++if null key then chain "_" (map name ats') else key))
>         maySee      = (mors context)     -- see everything
>         mayChange   = (mors context)     -- change everything
>      --    (IF Gemiddeld)
>         params      = [ Aspc (makeFSid1(varName (name a))) (handle context a) | a<-ats']
>         results     = []
>         invariants  = (dressRules
>                       [ (clause,rule)
>                       | (conj,rule)<-rs
>                       , clause@(Fu terms)<-[rClause conj]
>                       , not (null (mors o `isc` mors [t| t<-terms, isPos t]))])
>--    Pre (example:) 
>         preconds    = []
>--    Post (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
>         postconds   = ["\\hbox{\\tt obj}\\in"++idName o++"\\ \\hbox{implies that not}"++
>                        (if length (attributes o)==1 then let a=head (attributes o) in "\\ ("++tt ("obj."++name a)++"="++showADL (ctx a)++")" else
>                        "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
>                        chain "\\\\\n" ["&"++tt ("obj."++name a)++"="++tt (varName (name a))|a<-attributes o]++
>                        "&)\n\\end{array}$"
>                        )]
>         varName :: String -> String
>         varName = uName (map name (attributes o))

>  updateObj :: Context -> ObjectDef -> [Morphism] -> [(Expression,Rule)] -> ServiceSpec
>  updateObj context o cs rs
>   = Sspc fid maySee mayChange
>          -- No more FPA here
>          params results invariants preconds postconds 
>       where
>         fid         = makeFSid1 ("update_"++name o)
>         maySee      = (mors o)     -- see everything
>         mayChange   = (mors o)     -- change everything
>      --    (IF Gemiddeld)
>         params      = (Aspc (makeFSid1 "x") (handle context o): [ Aspc (makeFSid1(varName (name a))) (handle context a) | a<-attributes o])
>         results     = []
>         invariants  = (dressRules rs)
>--    Pre (example:) {Assume x left l, x right r, x src s, and x trg t}
>         preconds    = []
>--    Post (example:) {x left l, x right r, x src s, and x trg t}
>         postconds   = [ tt ("x."++name a)++"="++tt (varName (name a))|a<-attributes o]
>         varName :: String -> String
>         varName = uName (map name (attributes o))

>  deleteObj :: Context -> ObjectDef -> [(Expression,Rule)] -> ServiceSpec
>  deleteObj context o rs
>   = Sspc fid maySee mayChange
>          -- No more FPA here
>          params results invariants preconds postconds 
>       where
>         fid         = makeFSid1 ("delete_"++name o)
>         maySee      = [mIs (concept o)]     -- see everything
>         mayChange   = (mors o)              -- change everything
>      --    (IF Gemiddeld)
>         params      = [Aspc (makeFSid1 "x") (handle context o)] 
>         results     = []
>         invariants  = (dressRules
>                       [ (clause,rule)
>                       | (conj,rule)<-rs
>                       , clause@(Fu terms)<-[rClause conj]
>                       , not (null (mors o `isc` mors [t| t<-terms, isPos t]))])
>--    Pre
>         preconds    = [if length (attributes o)==1 then let a=head (attributes o) in tt ("x."++name a)++"="++idNam (nameAt a) else
>                        "$\\begin{array}[t]{lll}\n"++
>                        chain "\\\\\nand" ["&"++tt ("x."++name a)++"="++idNam (nameAt a)|a<-(attributes o)]++
>                        "&\n\\end{array}$"
>                       | not (null (attributes o))]
>--    Post 
>         postconds   = [if null (attributes o) then "\\hbox{\\tt x}\\not\\in"++idName (concept o) else
>                        "\\hbox{\\tt obj}\\in"++idName (concept o)++"\\ \\hbox{implies that not}"++
>                        (if length (attributes o)==1 then let a=head (attributes o) in "\\ ("++tt ("obj."++name a)++"="++idNam (nameAt a)++")" else
>                        "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
>                        chain "\\\\\nand" ["&"++tt ("obj."++name a)++"="++idNam (nameAt a)|a<-attributes o]++
>                        "&)\n\\end{array}$"
>                        )]

>  handle :: Identified a => Context -> a -> String
>  handle context c = firstCaps (name c)++if name c `elem` (map name entities) then "Handle" else ""
>   where (entities,relations,ruls) = erAnalysis context

>  dressRules :: [(Expression,Rule)] -> [Rule]
>  dressRules clauses = [ if length cl>1 then rule else makeRule rule clause | cl<-(map rd.eqCl snd) clauses, (clause,rule)<-take 1 cl]
>   where
>    f (Fu cl) (Fu cl') = [e| e<-cl, isPos e] `eq` [e| Cp e<-cl'] &&
>                         [e| Cp e<-cl] `eq` [e| e<-cl', isPos e]
>    self (Fu cl)       = [e| e<-cl, isPos e] `eq` [e| Cp e<-cl]
>    a `eq` b = length a==length b && length a==length (a `isc` b)


>  uName :: [String] -> String -> String
>  uName nms n = concat [v| (nm,v)<-zip nms' vs, n==nm]
>   where nms' = rd nms
>         vs = f (map (map toLower.take 1) nms')
>         f (v:vs)          = if v `elem` vs then f (g v (map show [1..]) (v:vs)) else v: f vs
>         f []              = []
>         g e (i:is) (v:vs) = if v==e then (v++i): g e is vs else v: g e (i:is) vs
>         g e _ []          = []

>  nameAt :: (Identified a, Object a) => a -> String
>  nameAt a = firstCaps ((map toLower.name.target.ctx) a++"_"++name a)
>  idName c = name c

>  makeFSid1 :: String -> FSid
>  makeFSid1 s = FS_id (firstCaps s)  -- We willen geen spaties in de naamgeveing.

