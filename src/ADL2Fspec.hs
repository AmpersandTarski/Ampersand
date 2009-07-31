  {-# OPTIONS_GHC -Wall #-}
  module ADL2Fspec (makeFspec)
  where
--   import CommonClasses ( Identified(name))
   import Collection     ( Collection (isc,(>-),rd) )
   import Auxiliaries    (sort',eqCl)
   import Strings        (firstCaps)
   import Adl            (Context(..)
                         ,ObjectDef(..)
                         ,Expression(..),notCp
                         ,Rule(..),normExpr
                         ,Pattern(..)
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
 -- The story:
 -- A number of datasets for this context is identified.
 -- Every pattern is considered to be a theme and every object is treated as a separate object specification.
 -- Every dataset is discussed in precisely one theme
 -- Every theme will be explained in a chapter of its own.

   makeFspec :: Context -> Fspc
   makeFspec context =
      Fspc { fsfsid = makeFSid1 (name context)
            , datasets = datasets' --TODO: datasets vervangen voor plug's
              -- serviceS contains the services defined in the ADL-script.
              -- services are meant to create user interfaces, programming interfaces and messaging interfaces.
              -- A generic user interface (the Monastir interface) is already available.
            , vplugs   = map makeSqlPlug (ctxsql context) ++ map makePhpPlug (ctxphp context)
            , serviceS = attributes context
            , serviceG = serviceG'
            , services = [makeFservice context a | a <-attributes context]
            , vrules   = rules context
            , vrels    = [makeFdecl d| d <-declarations context]
            , fsisa    = ctxisa context
            , vpatterns= patterns context
            } where
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
      sqltp obj = head $ [makeSqltype sqltp | strs<-objstrs obj,('S':'Q':'L':'T':'Y':'P':'E':'=':sqltp)<-strs]
                         ++[SQLVarchar 255]
      makeSqltype str = case str of
          ('V':'a':'r':'c':'h':'a':'r':xs) -> SQLVarchar 255 --TODO number
          ('C':'h':'a':'r':xs) -> SQLChar 255 --TODO number
          ('B':'l':'o':'b':xs) -> SQLBlob
          ('S':'i':'n':'g':'l':'e':xs) -> SQLSingle
          ('D':'o':'u':'b':'l':'e':xs) -> SQLDouble
          ('u':'I':'n':'t':xs) -> SQLuInt 4 --TODO number
          ('s':'I':'n':'t':xs) -> SQLsInt 4 --TODO number
          ('I':'d':xs) -> SQLId 
          ('B':'o':'o':'l':xs) -> SQLBool
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
      makeReturns = head $ [PhpReturn {retval=PhpObject{object=oa,phptype=makePhptype oa}}
                           | oa<-objats plug, strs<-objstrs oa,"PHPRETURN"<-strs]
                           ++ [PhpReturn {retval=PhpNull}]
      makeArgs = [(i,PhpObject{object=oa,phptype=makePhptype oa})
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

   makeFservice :: Context -> ObjectDef -> Fservice
   makeFservice _ obj
    = Fservice{
        objectdef = obj  -- the object from which the service is drawn
      }

   makeFSid1 :: String -> FSid
   makeFSid1 s = FS_id (firstCaps s)  -- We willen geen spaties in de naamgeveing.

   fst3 :: (a,b,c) -> a
   fst3 (a,_,_) = a
   snd3 :: (a,b,c) -> b
   snd3 (_,b,_) = b
