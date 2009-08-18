  {-# OPTIONS_GHC -Wall #-}
  module ADL2Fspec (makeFspec)
  where
   import Collection     ( Collection (rd) )
   import Strings        (firstCaps)
   import Adl            (Context(..)
                         ,ObjectDef(..)
                         ,Expression(..),notCp
                         ,Rule(..),normExpr,cpu
                         ,Morphism(..),makeDeclaration
                         ,Declaration(..)
                         ,Object(..)
                         ,Population(..)
                         ,Prop(..)
                         ,Concept(..)
                         ,Language(..)
                         ,FilePos(..)
                         ,Association(..),Morphic(..),Morphical(..)
                         )
   import Dataset
   import FspecDef
   import PredLogic
   import Languages
   import NormalForms(disjNF)
   import Data.Plug
   import Rendering.ClassDiagram
 -- The story:
 -- A number of datasets for this context is identified.
 -- Every pattern is considered to be a theme and every object is treated as a separate object specification.
 -- Every dataset is discussed in precisely one theme
 -- Every theme will be explained in a chapter of its own.

   makeFspec :: Context -> Fspc
   makeFspec context =
      Fspc { fsfsid = makeFSid1 (name context)
            , datasets = datasets'
              -- serviceS contains the services defined in the ADL-script.
              -- services are meant to create user interfaces, programming interfaces and messaging interfaces.
              -- A generic user interface (the Monastir interface) is already available.
            , vplugs   = vsqlplugs ++ vphpplugs
            , serviceS = attributes context
            , serviceG = serviceG'
            , services = [makeFservice context a | a <-attributes context]
            , vrules   = rules context
            , vrels    = [makeFdecl d| d <-declarations context]
            , fsisa    = ctxisa context
            , vpatterns= patterns context
            , classdiagrams = [cdAnalysis context True pat | pat<-patterns context]
            , themes = themes'
            } where
        vsqlplugs = map makeSqlPlug (ctxsql context)
        vphpplugs = map makePhpPlug (ctxphp context)
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
           preventAmbig mp = mp
           relsFrom c = [Mph (name d) Nowhere [] (source d,target d) True d| d@(Sgn {})<-declarations context, source d == c]++
                        [flp (Mph (name d) Nowhere [] (source d,target d) True d)| d@(Sgn {})<-declarations context, target d == c]
         -- recur :: [Morphism] -> Concept -> [Expression]
          -- recur rs' c
           --  = [ F [Tm mph| mph<-rs'++[n]] | n<-new, not (n `elem` rs')] ++
             --  [ rs'' | n<-new, not (n `elem` rs'), rs'' <-recur (rs'++[n]) (target n) ] 
               --where new = [mph| mph<-relsFrom c, not (isSignal mph), not (isIdent mph), Tot `elem` multiplicities mph]
           props ps = [if Sym `elem` ps && Asy `elem` ps then ["PROPERTY"] else
                       if Tot `elem` ps && Uni `elem` ps then ["ATTRIBUTE"] else
                       if Tot `elem` ps                  then ["NONEMPTY LIST"] else
                       if                  Uni `elem` ps then ["OPTIONAL FIELD"] else
                                                              ["LIST"]
                      ]
{- A dataset combines all functions that share the same source.
   This is used for function point analysis (in which data sets are counted).
   It can also be used in code generate towards SQL, allowing the code generator to
   implement relations wider than 2, for likely (but yet to be proven) reasons of efficiency.
   Datasets are constructed from the basic ontology (i.e. the set of relations with their multiplicities.) -}
        datasets'  = makeDatasets context
        makeFdecl d 
         = case d of
             Sgn{}     -> d{decpopu = rd( [link| Popu mph ps<-populations context, makeDeclaration mph==d, link<-ps]
                                          ++(decpopu d))
                           }
             Isn{}     -> d
             Iscompl{} -> d
             Vs{}      -> d
        --TODO -> assign themerules to themes and remove them from the Anything theme
        themes' = FTheme{tconcept=Anything,tfunctions=[],trules=themerules}
                  :(map maketheme$orderby [(wsopertheme oper, oper)
                                          |oper<-themeoperations, wsopertheme oper /= Nothing])
        --TODO -> by default CRUD operations of datasets, possibly overruled by SQL or PHP plugs
        themeoperations = datasetoperations++phpoperations++sqloperations
        phpoperations =[makeDSOperation$makePhpPlug phpplug | phpplug<-(ctxphp context)]
        sqloperations =[oper|obj<-(ctxsql context), oper<-makeDSOperations obj]
        datasetoperations = [oper|obj<-datasets', oper<-makeDSOperations obj, objtheme obj /=S]
        --query copied from FSpec.hs revision 174
        themerules = [r|p<-patterns context, r<-declaredRules p++signals p, null (cpu r)]
        maketheme (Just c,fs) = FTheme{tconcept=c,tfunctions=fs,trules=[]}
        maketheme _ = error $ "Error in ADL2Fspec.hs module ADL2Fspec function makeFspec.maketheme: "
                           ++ "The theme must involve a concept."
        orderby :: (Eq a) => [(a,b)] ->  [(a,[b])]
        orderby xs =  [(x,[y|(x',y)<-xs,x==x']) |x<-rd [dx|(dx,_)<-xs] ]
  
   makeSqlPlug :: ObjectDef -> Plug
   makeSqlPlug plug = PlugSql{fields=makeFields Nothing plug,database=CurrentDb,plname=name plug}
      where
      makeFields :: Maybe Expression -> ObjectDef -> [SqlField]
      makeFields mbexpr obj = 
          (Fld{fldname=name obj,fldexpr=fexpr,fldtype=sqltp obj,fldnull=False,flduniq=False})
          :[f | objat<-objats obj, f<-makeFields (Just fexpr) objat]
          where fexpr=case mbexpr of 
                          Nothing -> objctx obj
                          Just expr -> F [expr,objctx obj]
      sqltp :: ObjectDef -> SqlType
      sqltp obj = head $ [makeSqltype sqltp' | strs<-objstrs obj,('S':'Q':'L':'T':'Y':'P':'E':'=':sqltp')<-strs]
                         ++[SQLVarchar 255]
      makeSqltype :: String -> SqlType
      makeSqltype str = case str of
          ('V':'a':'r':'c':'h':'a':'r':_) -> SQLVarchar 255 --TODO number
          ('C':'h':'a':'r':_) -> SQLChar 255 --TODO number
          ('B':'l':'o':'b':_) -> SQLBlob
          ('S':'i':'n':'g':'l':'e':_) -> SQLSingle
          ('D':'o':'u':'b':'l':'e':_) -> SQLDouble
          ('u':'I':'n':'t':_) -> SQLuInt 4 --TODO number
          ('s':'I':'n':'t':_) -> SQLsInt 4 --TODO number
          ('I':'d':_) -> SQLId 
          ('B':'o':'o':'l':_) -> SQLBool
          _ -> SQLVarchar 255 --TODO number

   makePhpPlug :: ObjectDef -> Plug
   makePhpPlug plug = PlugPhp{args=makeArgs,returns=makeReturns,function=PhpAction{action=makeActiontype,on=[]}
                             ,phpfile="phpPlugs.inc.php",plname=name plug}
      where
      makeActiontype = head $ [case str of {"SELECT"->Read;
                                            "CREATE"->Create;
                                            "UPDATE"->Update;
                                            "DELETE"->Delete;
                                            _ -> error $ "Choose from ACTION=[SELECT|CREATE|UPDATE|DELETE].\n"  
                                                         ++ show (objpos plug)
                                           }
                     | strs<-objstrs plug,'A':'C':'T':'I':'O':'N':'=':str<-strs]
                     ++ [error $ "Specify ACTION=[SELECT|CREATE|UPDATE|DELETE] on phpplug.\n"  ++ show (objpos plug)]
      makeReturns = head $ [PhpReturn {retval=PhpObject{objectdf=oa,phptype=makePhptype oa}}
                           | oa<-objats plug, strs<-objstrs oa,"PHPRETURN"<-strs]
                           ++ [PhpReturn {retval=PhpNull}]
      makeArgs = [(i,PhpObject{objectdf=oa,phptype=makePhptype oa})
                 | (i,oa)<-zip [1..] (objats plug), strs<-(objstrs oa), elem "PHPARG" strs]
   makePhptype :: ObjectDef -> PhpType
   makePhptype objat = head $ [case str of {"String"->PhpString;
                                            "Int"->PhpInt;
                                            "Float"->PhpFloat;
                                            "Array"->PhpArray;
                                            _ -> error $ "Choose from PHPTYPE=[String|Int|Float|Array].\n"  
                                                        ++ show (objpos objat)
                                           }
                     | strs<-objstrs objat,'P':'H':'P':'T':'Y':'P':'E':'=':str<-strs]
                     ++ [error $ "Specify PHPTYPE=[String|Int|Float|Array] on PHPARG or PHPRETURN.\n"
                                 ++ show (objpos objat)]

   --DESCR -> Use for plugs that describe a single operation like PHP plugs
   makeDSOperation :: Plug -> WSOperation
   makeDSOperation PlugSql{} = error $ "Error in ADL2Fspec.hs module ADL2Fspec function makeDSOperation: "
                                    ++ "SQL plugs do not describe a single operation."
   makeDSOperation p@PlugPhp{} = 
       let nullval val = case val of
                         PhpNull    -> True
                         PhpObject{}-> False
           towsaction x = case x of {Create->WSCreate;Read->WSRead;Update->WSUpdate;Delete->WSDelete}
       in WSOper{wsaction=towsaction$action$function p
                 ,wsmsgin=[objectdf arg|(_,arg)<-args p,nullval$arg]
                 ,wsmsgout=[objectdf$retval$returns p|nullval$retval$returns p]
                 }
   --DESCR -> Use for objectdefs that describe all four CRUD operations like SQL plugs
   makeDSOperations :: ObjectDef -> [WSOperation]
   makeDSOperations od = [WSOper{wsaction=WSCreate,wsmsgin=[od],wsmsgout=[]}
                         ,WSOper{wsaction=WSRead,wsmsgin=[],wsmsgout=[od]}
                         ,WSOper{wsaction=WSUpdate,wsmsgin=[od],wsmsgout=[]}
                         ,WSOper{wsaction=WSDelete,wsmsgin=[od],wsmsgout=[]}]

   wsopertheme :: WSOperation -> Maybe Concept
   wsopertheme oper = 
      let msgthemes = map objtheme (wsmsgin oper++wsmsgout oper)
      in if samethemes msgthemes then Just$head msgthemes else Nothing
   
   --REMARK -> called samethemes because only used in this context
   samethemes :: (Eq a) => [a] -> Bool
   samethemes [] = False
   samethemes (_:[]) = True
   samethemes (c:c':cs) = if c==c' then samethemes (c':cs) else False

   --DESCR -> returns the concept on which the objectdef acts 
   objtheme :: ObjectDef -> Concept
   objtheme obj = case source$objctx obj of
      S -> let objattheme = [objtheme objat|objat<-objats obj]
           in if (not.null) objattheme then head objattheme
              else S
      c -> c

   makeFservice :: Context -> ObjectDef -> Fservice
   makeFservice _ obj
    = Fservice{
        objectdef = obj  -- the object from which the service is drawn
      }

   makeFSid1 :: String -> FSid
   makeFSid1 s = FS_id (firstCaps s)  -- We willen geen spaties in de naamgeveing.

--   fst3 :: (a,b,c) -> a
--   fst3 (a,_,_) = a
--   snd3 :: (a,b,c) -> b
--   snd3 (_,b,_) = b
