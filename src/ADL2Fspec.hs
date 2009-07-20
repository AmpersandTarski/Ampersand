  {-# OPTIONS_GHC -Wall #-}
  module ADL2Fspec (makeFspec)
  where
--   import CommonClasses ( Identified(name))
   import Collection     ( Collection (isc,(>-),rd) )
   import Auxiliaries    (sort',eqCl)
   import Char           (toLower)
   import Strings        (firstCaps,chain)
   import Adl            (Context(..)
                         ,ObjectDef(..)
                         ,KeyDef(..)
                         ,Expression(..),v,notCp,isPos
                         ,Rule(..),normExpr
                         ,Pattern(..)
                         ,Morphism(..),mIs,makeDeclaration
                         ,Declaration(..)
                         ,Object(..)
                         ,Population(..)
                         ,Prop(..),Key(..)
                         ,Concept(..),cptS
                         ,Language(..)
                         ,FilePos(..)
                         ,Association(..),Morphic(..),Morphical(..),MorphicId(..)
                         )
   import Dataset
   import ShowADL
   import FspecDef
   import Calc
   import PredLogic
   import Languages
   import NormalForms(disjNF,conjNF)
   import ComputeRule(conjuncts,allClauses)
 -- The story:
 -- A number of datasets for this context is identified.
 -- Every pattern is considered to be a theme and every object is treated as a separate object specification.
 -- Every dataset is discussed in precisely one theme
 -- Every theme will be explained in a chapter of its own.

   makeFspec :: Context -> Fspc
   makeFspec context =
      Fspc { fsfsid = makeFSid1 (name context)
            , themes   = themes'  --TODO: Bewijzen dat dit termineert!(dat doet het nu niet altijd...) -- TODO Aanpassen op nieuwe Document structuur
            , datasets = datasets' --TODO: datasets vervangen voor plug's
            , serviceS = serviceS' --was:[] TODO: Loop verwijderen uit generatie serviceS.
                                   --GMI: Welke loop? ServiceS is gewoon 'attributes context'. Zie TODO in ServiceG'?
            , serviceG = serviceG'
            , services = [] --was: fservices --TODO: Herstellen, en bewijzen dat dit termineert!
            , vrules   = frules'
            , vrels    = frels
            , fsisa    = isa'
            } where

-- Themes are made in order to get readable chapters in documentation. So a theme collects everything that
-- needs to be introduced in the same unit of text. For that purpose everything is allocated to a theme only once.
        themes'   = ( [makeFtheme context pat ds| (pat,ds)<-pats]                      -- one pattern yields one theme
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

     

        serviceG'
         = concat
           [ [ Obj { objnm   = name c
                   , objpos  = Nowhere
                   , objctx  = Tm $ I [c] c c True -- was: Tm $ V [cptS,c] (cptS,c)
                   , objats  = [ Obj { objnm  = name mph++name (target mph)
                                     , objpos = Nowhere
                                     , objctx = Tm (preventAmbig mph)
                                     , objats = let ats = [] --TODO -> disabled because it causes loop problems at -p option
                                                          --[ Obj { objnm = concat [name mph'| mph'<-morlist att]++name (target att)
                                                          --      , objpos = Nowhere
                                                          --      , objctx = att
                                                          --      , objats = []
                                                          --      , objstrs = [["DISPLAYTEXT", showADL att++" "++name (target att)]]++props (multiplicities att)
                                                          --      }
                                                          -- | att<-recur [] (target mph)]
                                                in if null ats then []
                                                   else (( Obj { objnm = name (target mph)
                                                               , objpos = Nowhere
                                                               , objctx = Tm $ I [target mph] (target mph) (target mph) True
                                                               , objats = []
                                                               , objstrs= []
                                                               }
                                                          ):ats)
                                     , objstrs = [["DISPLAYTEXT", name mph++" "++name (target mph)]]++props (multiplicities mph)
                                     }
                                | mph<-relsFrom c, not (isSignal mph)]++
                                [ Obj { objnm =  name (srrel s)
                                      , objpos = Nowhere
                                      , objctx = disjNF (notCp (if source s==c then normExpr (srsig s) else flp (normExpr (srsig s))))
                                      , objats = []
                                      , objstrs = [["DISPLAYTEXT", if null (srxpl s) then (lang English .assemble.normRule) (srsig s) else srxpl s]]
                                      }
                                | s<-signals context, source s==c || target s==c ]
                   , objstrs = []
                   }]
             ++let ats = [ Obj { objnm  = name mph++name (target mph)
                               , objpos = Nowhere
                               , objctx = Tm (preventAmbig mph)
                               , objats = []
                               , objstrs= [["DISPLAYTEXT", name mph++" "++name (target mph)]]++props (multiplicities mph)
                               }
                           | mph<-relsFrom c, not (isSignal mph), Tot `elem` multiplicities mph]
               in [ Obj { objnm  = name c++"s"
                        , objpos = Nowhere
                        , objctx = Tm $ I [S] S S True
                        , objats = [ Obj { objnm  = name c++"s"
                                         , objpos = Nowhere
                                         , objctx = Tm $ V [S,c] (S,c)
                                         , objats = ( Obj { objnm = "nr"
                                                          , objpos = Nowhere
                                                          , objctx = Tm $ I [c] c c True
                                                          , objats = []
                                                          , objstrs= []
                                                          }): ats
                                         , objstrs= []
                                         }
                                   ]
                        , objstrs = []
                        }
                        | not (null ats)
                  ]
           | c<-concs context ]
           where
           preventAmbig mp@(Mph{mphats=[]}) =  
              if (length [d|d@(Sgn {})<-declarations context, name mp==name d]) > 1
              then if mphyin mp 
                   then mp{mphats=[source mp,target mp]} 
                   else  mp{mphats=[target mp,source mp]}
              else mp 
           relsFrom c = [Mph (name d) Nowhere [] (source d,target d) True d| d@(Sgn {})<-declarations context, source d == c]++
                        [flp (Mph (name d) Nowhere [] (source d,target d) True d)| d@(Sgn {})<-declarations context, target d == c]
           recur :: [Morphism] -> Concept -> [Expression]
           recur rs' c
             = [ F [Tm mph| mph<-rs'++[n]] | n<-new, not (n `elem` rs')] ++
               [ rs'' | n<-new, not (n `elem` rs'), rs'' <-recur (rs'++[n]) (target n) ] 
               where new = [mph| mph<-relsFrom c, not (isSignal mph), not (isIdent mph), Tot `elem` multiplicities mph]
           props ps = [if Sym `elem` ps && Asy `elem` ps then ["PROPERTY"] else
                       if Tot `elem` ps && Uni `elem` ps then ["ATTRIBUTE"] else
                       if Tot `elem` ps                  then ["NONEMPTY LIST"] else
                       if                  Uni `elem` ps then ["OPTIONAL FIELD"] else
                                                              ["LIST"]
                      ]

-- serviceS contains the services defined in the ADL-script.
-- services are meant to create user interfaces, programming interfaces and messaging interfaces.
-- A generic user interface (the Monastir interface) is already available.
        serviceS' = attributes context
{- A dataset combines all functions that share the same source.
   This is used for function point analysis (in which data sets are counted).
   It can also be used in code generate towards SQL, allowing the code generator to
   implement relations wider than 2, for likely (but yet to be proven) reasons of efficiency.
   Datasets are constructed from the basic ontology (i.e. the set of relations with their multiplicities.) -}
        datasets'  = makeDatasets context
        isa'       = ctxisa context
        fservices  = [ makeFservice context a | a <-serviceS']
        frules'    = [ r | r <-rules context]
        frels      = [ makeFdecl context d| d <-declarations context]
        makeFdecl context d 
         = case d of
             Sgn{}     -> d{decpopu = rd( [link| Popu mph ps<-populations context, makeDeclaration mph==d, link<-ps]
                                          ++(decpopu d))
                           }
             Isn{}     -> d
             Iscompl{} -> d
             Vs{}      -> d

 -- next thing, we look which datasets will be discussed in which themes.
 -- Priority is given to those patterns that contain a concept definition of a root concept of the dataset,
 -- because we assume that the programmer found it important enough to define that concept in that pattern.
 -- in order to ensure that at most one pattern discusses a dataset, double (pat,cs,d)-triples are dropped.
        pcsds0 = (map (head.sort' snd3).eqCl (name.fst3))
                 [ (pat,length cns,ds)
                 | pat<-ctxpats context, ds<-datasets', cns<-map name (concs ds) `isc` [name c|c<-conceptDefs pat], not (null cns)]
 -- Now, pcsds0 covers concepts that are both root of a dataset and are defined in a pattern.
 -- The remaining concepts and datasets are determined in pcsds1.
 -- A dataset is assigned to the pattern with the most morphisms about the root(s) of the dataset.
        pcsds1 = (map (head.sort' snd3).eqCl (name.fst3))
                 [ (pat,0-length ms,ds)
                 | pat<-ctxpats context, ds <- datasets'>-[ds|(_,_,ds)<-pcsds0]
                 , ms<-[[mph|mph<-morlist pat, mph `elem` mors ds || flp mph `elem` mors ds]], not (null ms)
                 ]
 -- The remaining datasets will be discussed in the last theme
        remainingDS = datasets'>-[ds'|(_,_,ds')<-pcsds0++pcsds1]
        others
         = Pat { ptnm = "Other topics"
               , ptrls = []
               , ptgns = []
               , ptdcs = rd [d| ds<-remainingDS, d<-declarations ds]
               , ptcds = []
               , ptkds = []}

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
   --REMARK -> make a theme even if there aren't any datasets, because we do not want to throw these patterns
   makeFtheme context pat [] = Tspc fid [makeFunit context pat [] ] pat
      where fid = makeFSid1 (name pat)
   makeFtheme context pat dss -- dss zijn de datasets die afgeleid zijn van de context
    = Tspc fid units' pat
      where
        fid = makeFSid1 (name pat)
        units' = [makeFunit context pat (objs ds) | ds<-dss]
         where
          objs ds = [o| o<-attributes context, makeDataset context (concept o)==ds]

   makeFservice :: Context -> ObjectDef -> Fservice
   makeFservice context obj
    = Fservice{
        objectdef = obj  -- the object from which the service is drawn
      }

   makeFdecl :: Context -> Declaration -> Declaration
   makeFdecl context d 
        = case d of 
             Sgn{}     -> d{decpopu = rd ([link| Popu mph ps<-populations context, makeDeclaration mph==d, link<-ps]++(decpopu d))}
             Isn{}     -> d
             Iscompl{} -> d
             Vs{}      -> d 
    
--   d@(Sgn nm a b props prL prM prR cs expla pos nr sig)
--                     = (Sgn nm a b props prL prM prR cs' expla pos nr sig)
--      where cs' = rd ([link| Popu m ps<-populations context, makeDeclaration m==d, link<-ps]++cs)
--   makeFdecl context d = d

   makeFunit :: Context -> Pattern -> [ObjectDef] -> Funit
   makeFunit context pat objs 
    = Uspc fid pat 
        where
          fid  = (if null objs then (makeFSid1 "*NONAME*") else makeFSid1(name (head objs))) 

   handle :: Context -> ObjectDef -> String
   handle context c = firstCaps (name c)++if name c `elem` (map name (makeDatasets context)) then "Handle" else ""

   dressRules :: [(Expression,Rule)] -> [Rule]
   dressRules clauses = [ if length cl>1 then rule else makeRule rule clause | cl<-(map rd.eqCl snd) clauses, (clause,rule)<-take 1 cl]
--    where
--     f (Fu cl) (Fu cl') = [e| e<-cl, isPos e] `eq` [e| Cp e<-cl'] &&
--                          [e| Cp e<-cl] `eq` [e| e<-cl', isPos e]
--     self (Fu cl)       = [e| e<-cl, isPos e] `eq` [e| Cp e<-cl]
--     a `eq` b = length a==length b && length a==length (a `isc` b)


   uName :: [String] -> String -> String
   uName nms n = concat [v'| (nm,v')<-zip nms' vs, n==nm]
    where nms' :: [String] 
          nms' = rd nms
          vs :: [String]
          vs = f (map (map toLower.take 1) nms')
          f :: [String] -> [String]
          f (v':vs')          = if v' `elem` vs' then f (g v' (map show [1..]) (v':vs')) else v': f vs'
          f []              = []
          g :: String -> [String] -> [String] -> [String]
          g e' (i:is) (v':vs') = if v'==e' then (v'++i): g e' is vs' else v': g e' (i:is) vs'
          g _ _ []          = []
          g _ [] _          = undefined
          
   nameAt :: (Identified a, Object a) => a -> String
   nameAt a = firstCaps ((map toLower.name.target.ctx) a++"_"++name a)

   makeFSid1 :: String -> FSid
   makeFSid1 s = FS_id (firstCaps s)  -- We willen geen spaties in de naamgeveing.

   fst3 :: (a,b,c) -> a
   fst3 (a,_,_) = a
   snd3 :: (a,b,c) -> b
   snd3 (_,b,_) = b
   thd3 :: (a,b,c) -> c
   thd3 (_,_,c) = c

   idNam :: String -> String
   idNam c = "\\id{"++concat [if c `elem` [' ','_'] then "\\ " else [c]| c<-firstCaps c]++"}"

