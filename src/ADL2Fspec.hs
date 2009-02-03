  module ADL2Fspec (makeFspec)
  where
   import CommonClasses ( Identified(name))
   import Collection    ( Collection (isc,(>-),rd) )
   import Auxiliaries(sort,sort',snd3,eqCl,fst3,eqClass)
   import Char(toLower)
   import Strings(firstCaps,idNam,chain)
   import Adl
   import Dataset
   import ShowADL
   import FspecDef
   import Calc
   import PredLogic
   import Languages

 -- The story:
 -- A number of datasets for this context is identified.
 -- Every pattern is considered to be a theme and every object is treated as a separate object specification.
 -- Every dataset is discussed in precisely one theme
 -- Every theme will be explained in a chapter of its own.

   makeFspec :: Context -> Fspc
   makeFspec context
     = Fspc fid themes datasets serviceS serviceG fservices frules frels isa where
        fid      = makeFSid1 (name context)
-- Themes are made in order to get readable chapters in documentation. So a theme collects everything that
-- needs to be introduced in the same unit of text. For that purpose everything is allocated to a theme only once.
        themes   = (  [makeFtheme context pat ds| (pat,ds)<-pats]                      -- one pattern yields one theme
                   ++ [makeFtheme context others remainingDS| not (null remainingDS)]  -- remaining datasets are discussed at the end
                   )
-- services (type ObjectDef) can be generated from a basic ontology. That is: they can be derived from a set
-- of relations together with multiplicity constraints. That is what serviceG does.
-- This is meant to help a developer to build his own list of services, by providing a set of services that works.
-- The developer will want to assign his own labels and maybe add or rearrange attributes.
-- This is easier than to invent a set of services from scratch.
-- At a later stage, serviceG will be used to generate semantic error messages. The idea is to compare a service
-- definition from the ADL-script with the generated service definition and to signal missing items.
-- Rule: a service must be large enough to allow the required transactions to take place within that service.
-- TODO: afdwingen dat attributen van elk object unieke namen krijgen.
        serviceG
         = concat
           [ [ (objdefNew (v (cptS,c)))
                  { objnm  = name c
                  , objats = [ (objdefNew (Tm m))
                                  { objnm  = name m++name (target m)
                                  , objstrs = [["DISPLAYTEXT", name m++" "++name (target m)]]++props (multiplicities m)
                                  , objats = let ats = [ (objdefNew att) { objnm = concat [name m| m<-morlist att]++name (target att)
                                                                         , objstrs = [["DISPLAYTEXT", showADL att++" "++name (target att)]]++props (multiplicities att)
                                                                         }
                                                       | att<-recur [] (target m)]
                                             in if null ats then []
                                                else ((objdefNew (Tm (mIs (target m))))
                                                         { objnm = name (target m) }):ats
                                  }
                             | m<-relsFrom c, not (isSignal m)]++
                             [ ((objdefNew . disjNF . notCp) (if source s==c then normExpr (srsig s) else flp (normExpr (srsig s))))
                                  { objnm   = name (srrel s)
                                  , objstrs = [["DISPLAYTEXT", if null (srxpl s) then (lang English .assemble.normRule) (srsig s) else srxpl s]]
                                  }
                             | s<-signals context, source s==c || target s==c ]
                  }]
             ++let ats = [ (objdefNew (Tm m))
                              { objnm   = name m++name (target m)
                              , objstrs = [["DISPLAYTEXT", name m++" "++name (target m)]]++props (multiplicities m)
                              , objats  = []
                              }
                         | m<-relsFrom c, not (isSignal m), Tot `elem` multiplicities m]
               in [(objdefNew (Tm (mIs S)))
                     { objnm  = name c++"s"
                     , objats = [ (objdefNew (v(S,c)))
                                     { objnm  = name c++"s"
                                     , objats = ((objdefNew (Tm (mIs c))) { objnm = "nr" }): ats
                                     } ]
                     }| not (null ats)]
           | c<-concs context ]
           where
            relsFrom c = [Mph (name d) posNone [] (source d,target d) True d| d@(Sgn {})<-declarations context, source d == c]++
                         [flp (Mph (name d) posNone [] (source d,target d) True d)| d@(Sgn {})<-declarations context, target d == c]
            recur :: [Morphism] -> Concept -> [Expression]
            recur rs c
             = [ F [Tm m| m<-rs++[n]] | n<-new, not (n `elem` rs)] ++
               [ rs' | n<-new, not (n `elem` rs), rs' <-recur (rs++[n]) (target n) ] 
               where new = [m| m<-relsFrom c, not (isSignal m), not (isIdent m), Tot `elem` multiplicities m]
            props ps = [if Sym `elem` ps && Asy `elem` ps then ["PROPERTY"] else
                        if Tot `elem` ps && Uni `elem` ps then ["ATTRIBUTE"] else
                        if Tot `elem` ps                  then ["NONEMPTY LIST"] else
                        if                  Uni `elem` ps then ["OPTIONAL FIELD"] else
                                                               ["LIST"]
                       ]

-- serviceS contains the services defined in the ADL-script.
-- services are meant to create user interfaces, programming interfaces and messaging interfaces.
-- A generic user interface (the Monastir interface) is already available.
        serviceS = attributes context
{- A dataset combines all functions that share the same source.
   This is used for function point analysis (in which data sets are counted).
   It can also be used in code generate towards SQL, allowing the code generator to
   implement relations wider than 2, for likely (but yet to be proven) reasons of efficiency.
   Datasets are constructed from the basic ontology (i.e. the set of relations with their multiplicities.) -}
        datasets  = makeDatasets context
        fservices = [ makeFservice context a | a <-serviceS]
        frules    = [ r | r <-rules context]
        frels     = [ {- makeFdecl context -} d | d <-declarations context] -- TODO: makeFdecl wordt nu nog in ADLdef aangeroepen. Wanneer de SQL-objecten eenmaal vanuit de Fspc worden gegenereerd, moet makeFdecl natuurlijk op deze plaats worden aangeroepen...
        isa       = ctxisa context

 -- next thing, we look which datasets will be discussed in which themes.
 -- Priority is given to those patterns that contain a concept definition of a root concept of the dataset,
 -- because we assume that the programmer found it important enough to define that concept in that pattern.
 -- in order to ensure that at most one pattern discusses a dataset, double (pat,cs,d)-triples are dropped.
        pcsds0 = (map (head.sort' snd3).eqCl (name.fst3))
                 [ (pat,length cns,ds)
                 | pat<-ctxpats context, ds<-datasets, cns<-map name (concs ds) `isc` [name c|c<-conceptDefs pat], not (null cns)]
 -- Now, pcsds0 covers concepts that are both root of a dataset and are defined in a pattern.
 -- The remaining concepts and datasets are determined in pcsds1.
 -- A dataset is assigned to the pattern with the most morphisms about the root(s) of the dataset.
        pcsds1 = (map (head.sort' snd3).eqCl (name.fst3))
                 [ (pat,0-length ms,ds)
                 | pat<-ctxpats context, ds <- datasets>-[ds|(_,_,ds)<-pcsds0]
                 , ms<-[[m|m<-morlist pat, m `elem` mors ds || flp m `elem` mors ds]], not (null ms)
                 ]
 -- The remaining datasets will be discussed in the last theme
        remainingDS = datasets>-[ds'|(_,_,ds')<-pcsds0++pcsds1]
        others
         = Pat "Other topics" rs gen pms cs ks
           where rs  = []
                 gen = []
                 pms = rd [d| ds<-remainingDS, d<-declarations ds]
                 cs  = []
                 ks  = []

    --    context' = Ctx nm on i world pats rs ds cs ks os pops
    --       where nm    = name context
    --             on    = extends context
    --             i     = isa context
    --             world = wrld context
    --             pats  = ctxpats context ++ if null remainingDS then [] else [others]
    --             rs    = ctxrs context
    --             ds    = declarations context
    --             cs    = conceptDefs context
    --             ks    = []
    --             os    = attributes context
    --             pops  = populations context
 -- The patterns with the appropriate datasets are determined:
        pats = [ (pat, [dg| (p,_,dg)<-pcsds0++pcsds1, name pat==name p]) | pat<-ctxpats context]

   makeFtheme :: Context -> Pattern -> [ObjectDef] -> Ftheme
   makeFtheme context pat dss -- dss zijn de datasets die afgeleid zijn van de context
    = Tspc fid units pat
      where
        fid = makeFSid1 (name pat)
        units = [makeFunit context pat (objs ds) [] []| ds<-dss]
         where
          objs ds = [o| o<-attributes context, makeDataset context (concept o)==ds]

   makeFservice :: Context -> ObjectDef -> Fservice
   makeFservice context o
    = Fservice
        o                                  -- the object from which the service is drawn
        trBound                            -- the transaction boundary, i.e. all expressions that may be changed in a transaction
        [ limit trBound eca                -- all ECA-rules that may be used in this object
        | rule<-declaredRules context
        , conjunct<-conjuncts rule
        , clause<-allClauses conjunct
        , eca<-doClause clause
        ]
        (makeDataset context (concept o))  -- the dataset in which the objects are stored
-- obsolete services?
        [ getEach context o
        , createObj context o [] {-rs-}
        , readObj context o
        , deleteObj context o [] {-rs-}
        , updateObj context o [] {-cs-} [] {-rs-} ]
        [ r| r<-rules context, not (null (mors r `isc` mors o))]  -- include all valid rules that relate directly to o.
      where
       trBound
        = rd [conjNF e | a<-atts o, e<-[objctx a, flp (objctx a)] ]
          where atts o = o: [e| a<-attributes o, e<-atts a]
       limit rels (ECA ev clause) = ECA ev (simplPAclause (lim clause))
        where
         lim (Choice clauses)              = Choice [c'| c<-clauses, c'<-[lim c], p c']
          where p (Do insdel toExpr delta) = conjNF toExpr `elem` trBound
                p (Choice [])              = False
                p _                        = True
         lim (All clauses)
            | null [ 1 | Choice []<-cls]   = All cls
            | otherwise                    = Choice []
          where
           cls = [lim c| c<-clauses]
         lim (Do insdel toExpr delta) 
            | conjNF toExpr `elem` trBound = Do insdel toExpr delta
            | otherwise                    = Choice []
         lim (New c)                       = New c


   makeFdecl :: Context -> Declaration -> Declaration
   makeFdecl context d@(Sgn nm a b props prL prM prR cs expla pos nr sig)
    = (Sgn nm a b props prL prM prR cs' expla pos nr sig)
      where cs' = rd ([link| Popu m ps<-populations context, makeDeclaration m==d, link<-ps]++cs)
   makeFdecl context d = d

   makeFunit :: Context -> Pattern -> [ObjectDef] -> [Concept] -> [ServiceSpec] -> Funit
   makeFunit context pat objs newConcs newDecls
    = Uspc fid pat ents svs
        where
          fid  = (if null objs then NoName else makeFSid1(name (head objs))) 
          ents = [ Vdef o
                -- ,ILGV Eenvoudig
                   [] {-cs-}
                   [] {-rs-}
                   | o<-objs]
          svs  = (concat [   [ createObj context o [] {-rs-} ]
                          ++ [ readObj context o]
                          ++ concat [ [keyEnt context o (key,ks), delKeyEnt context o (key,ks) [] {-rs-}]
                                      | (e,key,ks)<-keys pat, e==concept o]
                          ++ [ deleteObj context o [] {-rs-} ]
                          ++ [ updateObj context o [] {-cs-} [] {-rs-}| not (null [] {-cs-}) ]
                     | o<-objs ])



   getEach :: Context -> ObjectDef -> ServiceSpec 
   getEach context o
    = Sspc fid maySee mayChange
           -- No more FPA here
           params results invariants preconds postconds 
        where
          fid         = makeFSid1 ("getEach_"++name o)
          maySee      = [mIs (concept o)]  -- see  concept o  only.
          mayChange   = []                 -- change nothing
          params      = []                 -- input parameters
          results     = [ Aspc (makeFSid1 "objs") ("["++handle context o++"]")]  -- results
          invariants  = []                 -- rules
          preconds    = []                 -- Precondition
          postconds   = (["objs"++"= I["++name (concept o)++"]"]) -- Postcondition

   createObj :: Context -> ObjectDef -> [(Expression,Rule)] -> ServiceSpec 
   createObj context o rs
    = Sspc fid maySee mayChange
           -- No more FPA here
           params results invariants preconds postconds 
        where
          fid         = makeFSid1 ("create_"++name o)
          maySee      = ([mIs (concept o)]++mors o)      -- see the morphisms touched by this object
          mayChange   = (mors o)                         -- change these morphisms
      --     (IF Gemiddeld)
          params      = [ Aspc (makeFSid1(varName (name a))) (handle context a) | a<-attributes o]  -- input parameters
          results     = [ Aspc (makeFSid1("obj")) (handle context o)] -- results
          invariants  = (dressRules
                         [ (clause,rule)
                         | (conj,rule)<-rs
                         , clause@(Fu terms)<-[lClause conj]
                         , not (null (mors o `isc` mors [t| Cp t<-terms]))])
          preconds    =  []
 --    Post (example:) {o in Pair and o left l and o right r}
          postconds   =  ([("obj."++name a)++"="++(varName (name a))|a<-attributes o])
          varName :: String -> String
          varName = uName (map name (attributes o))
   readObj :: Context -> ObjectDef -> ServiceSpec
   readObj context o
    = Sspc fid maySee mayChange
           -- No more FPA here
           params results invariants preconds postconds 
        where
          fid         = makeFSid1 ("read_"++name o)
          maySee      = ([mIs (concept o)]++mors o) -- sees
          mayChange   = []                          -- change nothing
       --    (OF Eenvoudig)
          params      = [Aspc (makeFSid1 "x") (handle context o)]
          results     = [Aspc (makeFSid1 (varName (name a))) (handle context a) | a<-attributes o]
          invariants  = []
 --    Pre (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
          preconds    = ([ ("x."++name a)++"="++idNam (nameAt a) |a<-attributes o])
 --    Post (example:) {left=l, right=r, src=s, and trg=t}
          postconds   = [(varName (name a))++"="++idNam (nameAt a)|a<-attributes o]
          varName :: String -> String
          varName = uName (map name (attributes o))

   keyEnt :: Context -> ObjectDef -> (String,[ObjectDef]) -> ServiceSpec
   keyEnt context o (key,ats')
    = Sspc fid maySee mayChange
           -- No more FPA here
           params results invariants preconds postconds 
        where
          fid         = makeFSid1 ("sel_"++name o++"_by_"++if null key then chain "_" (map name ats') else key)
     --     serviceName = (firstCaps ("sel"++name o++"_by_"++if null key then chain "_" (map name ats') else key))
          maySee      = (mors context) -- see everything
          mayChange   = []             -- change nothing
      --     (OF Eenvoudig)
          params      = [ Aspc (makeFSid1(varName (name a))) (handle context a) | a<-ats']
          results     = [Aspc (makeFSid1 "obj") (handle context o)]
          invariants  = []
 --    Pre (example:) {Assume l=atom_left and r=atom_right}
          preconds    = [let args = [("x."++name a)++"="++(varName (name a)) | a<-attributes o] ++
                                    [("x."++name a)++"="++idNam (nameAt a)      | a<-attributes o, not (name a `elem` map name ats')]
                         in
                         "\\hbox{There is an {\\tt x}}\\in"++idName (concept o)++"\\ \\hbox{such that}"++
                         (if length args==1 then "\\ "++concat args else
                          "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
                          chain "\\\\\n" (map ('&':) args)++
                          "&)\n\\end{array}$"
                         )]
 --    Post (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
          postconds   = ([ "obj"++"="++"x"])
          varName :: String -> String
          varName = uName (map name (attributes o))

   delKeyEnt :: Context -> ObjectDef -> (String,[ObjectDef]) -> [(Expression,Rule)] -> ServiceSpec
   delKeyEnt context o (key,ats') rs
    = Sspc fid maySee mayChange
           -- No more FPA here
           params results invariants preconds postconds 
        where
          fid         = makeFSid1 ("del_"++name o++"_by_"++if null key then chain "_" (map name ats') else key)
       --   serviceName = (firstCaps ("del"++name o++"_by_"++if null key then chain "_" (map name ats') else key))
          maySee      = (mors context)     -- see everything
          mayChange   = (mors context)     -- change everything
       --    (IF Gemiddeld)
          params      = [ Aspc (makeFSid1(varName (name a))) (handle context a) | a<-ats']
          results     = []
          invariants  = (dressRules
                        [ (clause,rule)
                        | (conj,rule)<-rs
                        , clause@(Fu terms)<-[rClause conj]
                        , not (null (mors o `isc` mors [t| t<-terms, isPos t]))])
 --    Pre (example:) 
          preconds    = []
 --    Post (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
          postconds   = ["\\hbox{\\tt obj}\\in"++idName o++"\\ \\hbox{implies that not}"++
                         (if length (attributes o)==1 then let a=head (attributes o) in "\\ ("++("obj."++name a)++"="++showADL (ctx a)++")" else
                         "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
                         chain "\\\\\n" ["&"++("obj."++name a)++"="++(varName (name a))|a<-attributes o]++
                         "&)\n\\end{array}$"
                         )]
          varName :: String -> String
          varName = uName (map name (attributes o))

   updateObj :: Context -> ObjectDef -> [Morphism] -> [(Expression,Rule)] -> ServiceSpec
   updateObj context o cs rs
    = Sspc fid maySee mayChange
           -- No more FPA here
           params results invariants preconds postconds 
        where
          fid         = makeFSid1 ("update_"++name o)
          maySee      = (mors o)     -- see everything
          mayChange   = (mors o)     -- change everything
       --    (IF Gemiddeld)
          params      = (Aspc (makeFSid1 "x") (handle context o): [ Aspc (makeFSid1(varName (name a))) (handle context a) | a<-attributes o])
          results     = []
          invariants  = (dressRules rs)
 --    Pre (example:) {Assume x left l, x right r, x src s, and x trg t}
          preconds    = []
 --    Post (example:) {x left l, x right r, x src s, and x trg t}
          postconds   = [ ("x."++name a)++"="++(varName (name a))|a<-attributes o]
          varName :: String -> String
          varName = uName (map name (attributes o))

   deleteObj :: Context -> ObjectDef -> [(Expression,Rule)] -> ServiceSpec
   deleteObj context o rs
    = Sspc fid maySee mayChange
           -- No more FPA here
           params results invariants preconds postconds 
        where
          fid         = makeFSid1 ("delete_"++name o)
          maySee      = [mIs (concept o)]     -- see everything
          mayChange   = (mors o)              -- change everything
       --    (IF Gemiddeld)
          params      = [Aspc (makeFSid1 "x") (handle context o)] 
          results     = []
          invariants  = (dressRules
                        [ (clause,rule)
                        | (conj,rule)<-rs
                        , clause@(Fu terms)<-[rClause conj]
                        , not (null (mors o `isc` mors [t| t<-terms, isPos t]))])
 --    Pre
          preconds    = [if length (attributes o)==1 then let a=head (attributes o) in ("x."++name a)++"="++idNam (nameAt a) else
                         "$\\begin{array}[t]{lll}\n"++
                         chain "\\\\\nand" ["&"++("x."++name a)++"="++idNam (nameAt a)|a<-(attributes o)]++
                         "&\n\\end{array}$"
                        | not (null (attributes o))]
 --    Post 
          postconds   = [if null (attributes o) then "\\hbox{\\tt x}\\not\\in"++idName (concept o) else
                         "\\hbox{\\tt obj}\\in"++idName (concept o)++"\\ \\hbox{implies that not}"++
                         (if length (attributes o)==1 then let a=head (attributes o) in "\\ ("++("obj."++name a)++"="++idNam (nameAt a)++")" else
                         "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
                         chain "\\\\\nand" ["&"++("obj."++name a)++"="++idNam (nameAt a)|a<-attributes o]++
                         "&)\n\\end{array}$"
                         )]

   handle :: Context -> ObjectDef -> String
   handle context c = firstCaps (name c)++if name c `elem` (map name (makeDatasets context)) then "Handle" else ""

   dressRules :: [(Expression,Rule)] -> [Rule]
   dressRules clauses = [ if length cl>1 then rule else makeRule rule clause | cl<-(map rd.eqCl snd) clauses, (clause,rule)<-take 1 cl]
    where
     f (Fu cl) (Fu cl') = [e| e<-cl, isPos e] `eq` [e| Cp e<-cl'] &&
                          [e| Cp e<-cl] `eq` [e| e<-cl', isPos e]
     self (Fu cl)       = [e| e<-cl, isPos e] `eq` [e| Cp e<-cl]
     a `eq` b = length a==length b && length a==length (a `isc` b)


   uName :: [String] -> String -> String
   uName nms n = concat [v| (nm,v)<-zip nms' vs, n==nm]
    where nms' = rd nms
          vs = f (map (map toLower.take 1) nms')
          f (v:vs)          = if v `elem` vs then f (g v (map show [1..]) (v:vs)) else v: f vs
          f []              = []
          g e (i:is) (v:vs) = if v==e then (v++i): g e is vs else v: g e (i:is) vs
          g e _ []          = []

   nameAt :: (Identified a, Object a) => a -> String
   nameAt a = firstCaps ((map toLower.name.target.ctx) a++"_"++name a)
   idName c = name c

   makeFSid1 :: String -> FSid
   makeFSid1 s = FS_id (firstCaps s)  -- We willen geen spaties in de naamgeveing.

