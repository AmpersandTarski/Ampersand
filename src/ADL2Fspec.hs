  {-# OPTIONS_GHC -Wall #-}
  module ADL2Fspec (makeFspec)
  where
   import Collection     (Collection(rd,uni,isc,(>-)))
   import Strings        (firstCaps)
   import Adl
   import Auxiliaries    (naming, eqCl, eqClass, sort')
   import FspecDef
   import Languages
   import Calc
   import Options        (Options(language))
   import NormalForms(conjNF,disjNF,normECA)
   import Data.Plug
   import Char
   
   makeFspec :: Options -> Context -> Fspc
   makeFspec flags context = fSpec where
        allQuads = quads (\_->True) (rules context)
        fSpec =
            Fspc { fsName       = firstCaps (name context)
                   -- serviceS contains the services defined in the ADL-script.
                   -- services are meant to create user interfaces, programming interfaces and messaging interfaces.
                   -- A generic user interface (the Lonneker interface) is already available.
                 , vplugs       = definedplugs
                 , plugs        = allplugs
                 , serviceS     = attributes context -- services specified in the ADL script
                 , serviceG     = serviceG'          -- generated services
                 , services     = [ makeFservice context allQuads a | a <-attributes context++serviceG']
                 , vrules       = rules context++signals context
                 , vconjs       = rd [conj| Quad _ ccrs<-allQuads, (conj,_)<-cl_conjNF ccrs]
                 , vquads       = allQuads
--                 , ecaRules     = []
                 , vrels        = [ d{decprps = decprps d `uni` [Tot|m<-totals, d==makeDeclaration m, inline m]
                                                          `uni` [Sur|m<-totals, d==makeDeclaration m, not (inline m)]}
                                  | d<-declarations context]
                 , fsisa        = ctxisa context
                 , vpatterns    = patterns context
                 , pictPatts    = Nothing
                 , vConceptDefs = conceptDefs context
                 , pictConcepts = Nothing
                 , pictCD       = Nothing
                 , pictSB       = Nothing
                 , themes       = themes'
                 , violations   = [(r,viol) |r<-rules context, viol<-ruleviols r]
                 , vctxenv      = ctxenv context
                 }
        ruleviols (Ru{rrsrt=rtyp,rrant=ant,rrcon=con}) 
            | rtyp==Truth = contents$Cp con --everything not in con
            | rtyp==Implication = ant `contentsnotin` con 
            | rtyp==Equivalence = ant `contentsnotin` con ++ con `contentsnotin` ant 
            where
            contentsnotin x y = [p|p<-contents x, not$elem p (contents y)]
        ruleviols _ = [] 
        definedplugs = vsqlplugs ++ vphpplugs
-- maybe useful later...
--        conc2plug :: Concept -> Plug
--        conc2plug c = PlugSql {plname=name c, fields = [field (name c) (Tm (mIs c)) Nothing False True], plfpa = ILGV Eenvoudig}

-- mor2plug creates associations between plugs that represent wide tables.
-- this concerns relations that are not univalent nor injective,
-- Univalent and injective relations cannot be associations, as they are used as attributes in wide tables.
        mor2plug :: Morphism -> Plug
        mor2plug  m'
         = if Inj `elem` mults || Uni `elem` mults then error ("!Fatal (module ADL2Fspec 64): unexpected call of mor2plug("++show m'++"), because it is injective or univalent.") else
           if is_Tot
           then PlugSql { plname = name m'
                        , fields = [field (name (source m')) (Tm (mIs (source m'))) Nothing (not is_Sur) False {- isUni -}
                                   ,field (name (target m')) (Tm m') Nothing (not is_Tot) False {- isInj -}]
                        , plfpa  = NO
                        }
           else if is_Sur then mor2plug (flp m')
           else PlugSql { plname = name m'
                        , fields = [field (name (source m')) (Fi [Tm (mIs (source m')),F [Tm m',flp (Tm m')]]   -- WAAROM (SJ) is dit de expressie in dit veld?
                                                           )      Nothing (not is_Sur) False {- isUni -}
                                   ,field (name (target m')) (Tm m') Nothing (not is_Tot) False {- isInj -}]
                        , plfpa  = NO
                        }
           where
             mults = multiplicities m'
             is_Tot = Tot `elem` mults || m' `elem` totals
             is_Sur = Sur `elem` mults || flp m' `elem` totals
        totals :: [Morphism]
        totals
         = rd [ m | q<-quads visible (rules fSpec), isIdent (qMorph q)
                  , (_,hcs)<-cl_conjNF (qClauses q), Fu fus<-hcs
                  , antc<-[(conjNF.Fi) [notCp f| f<-fus, isNeg f]], isIdent antc
                  , f<-fus, isPos f
                  , m<-tots f
                  ]
           where tots (F fs) = [m| Tm m<-take 1 fs]++[flp m| Tm m<-take 1 (reverse fs)]
                 tots _ = []
                 visible _ = True -- for computing totality, we take all quads into account.

        allplugs = uniqueNames []
                    (definedplugs ++                  -- all plugs defined by the user
                     gPlugs       ++                  -- all plugs generated by the compiler
                     relPlugs                         -- all plugs for relations not touched by definedplugs and gplugs
                --     [conc2plug S]                  -- the universal singleton
                    )
          where
           gPlugs   = makePlugs context definedplugs
           relPlugs = map mor2plug ( map makeMph (declarations context)>-(mors definedplugs++mors gPlugs) )

        uniqueNames :: [String]->[Plug]->[Plug]
        -- MySQL is case insensitive! (hence the lowerCase)
        uniqueNames given plgs = naming (\x y->x{plname=y}) -- renaming function for plugs
                                        (map ((.) lowerCase) -- functions that name a plug (lowercase!)
                                             (name:n1:n2:[(\x->lowerCase(name x ++ show n))
                                                         |n<-[(1::Integer)..]])
                                        )
                                        (map lowerCase given) -- the plug-names taken
                                        (map uniqueFields plgs)
          where n1 p = name p ++ plsource p
                n2 p = name p ++ pltarget p
                plsource p = name (source (fldexpr (head (fields (p)))))
                pltarget p = name (target (fldexpr (last (fields (p)))))
                uniqueFields plug = plug{fields = naming (\x y->x{fldname=y}) -- renaming function for fields
                                                  (map ((.) lowerCase) -- lowercase-yielding
                                                       (fldname:[(\x->fldname x ++ show n)
                                                                |n<-[(1::Integer)..]])
                                                  )
                                                  [] -- no field-names are taken
                                                  (fields plug)
                                        }
                lowerCase = map toLower -- from Char
        vsqlplugs = map makeSqlPlug (ctxsql context)
        vphpplugs = map makePhpPlug (ctxphp context)
        -- services (type ObjectDef) can be generated from a basic ontology. That is: they can be derived from a set
        -- of relations together with multiplicity constraints. That is what serviceG does.
        -- This is meant to help a developer to build his own list of services, by providing a set of services that works.
        -- The developer will want to assign his own labels and maybe add or rearrange attributes.
        -- This is easier than to invent a set of services from scratch.

        -- Rule: a service must be large enough to allow the required transactions to take place within that service.
        -- TODO: afdwingen dat attributen van elk object unieke namen krijgen.
        serviceG'
         = concat
           [ [ Obj { objnm   = name c
                   , objpos  = Nowhere
                   , objctx  = Tm $ mIs c -- was: Tm $ V [cptS,c] (cptS,c)
                   , objats  = [ recur [] mph | mph<-relsFrom c]++
                                [ Obj { objnm =  name (srrel s)
                                      , objpos = Nowhere
                                      , objctx = disjNF (notCp (if source s==c then normExpr (srsig s) else flp (normExpr (srsig s))))
                                      , objats = []
                                      , objstrs = [] -- [["DISPLAYTEXT", if null (srxpl s) then (lang lng .assemble.normRule) (srsig s) else srxpl s]]
                                      }
                                | s<-signals context, source s==c || target s==c ]
                   , objstrs = []
                   }]++
             [ Obj { objnm   = (if language flags==Dutch then "nieuwe" else "new")++name c
                   , objpos  = Nowhere
                   , objctx  = Tm $ mIs c -- was: Tm $ V [cptS,c] (cptS,c)
                   , objats  = [ Obj { objnm =  name mph
                                     , objpos = Nowhere
                                     , objctx = Tm mph
                                     , objats = []
                                     , objstrs = [] -- [["DISPLAYTEXT", if null (srxpl s) then (lang lng .assemble.normRule) (srsig s) else srxpl s]]
                                     }
                               | mph<-fats c ]
                   , objstrs = []
                   } | not (null (fats c))]
             ++let ats = [ Obj { objnm  = composedname mph
                               , objpos = Nowhere
                               , objctx = Tm (preventAmbig mph)
                               , objats = []
                               , objstrs= [] -- [["DISPLAYTEXT", name mph++" "++name (target mph)]]++props (multiplicities mph)
                               }
                         | mph<-fats c]
               in [ Obj { objnm  = plural (language flags) (name c)
                        , objpos = Nowhere
                        , objctx = Tm $ mIs S
                        , objats = [ Obj { objnm  = plural (language flags) (name c)
                                         , objpos = Nowhere
                                         , objctx = Tm $ V [S,c] (S,c)
                                         , objats = ( Obj { objnm = "nr"
                                                          , objpos = Nowhere
                                                          , objctx = Tm $ mIs c
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
           | c<-concs context, null [a| a<-attributes context, target (ctx a)==c ]
           ]
           where
           fats c = [mph| mph<-relsFrom c, not (isSignal mph), isTot mph, isUni mph]
           composedname mph | inline mph = name mph++name (target mph)
                            | otherwise  = name (target mph) ++ "_of_" ++ name mph
           preventAmbig mp@(Mph{mphats=[]}) =  
              if (length [d|d@(Sgn {})<-declarations context, name mp==name d]) > 1
              then if mphyin mp 
                   then mp{mphats=[source mp,target mp]} 
                   else  mp{mphats=[target mp,source mp]}
              else mp 
           preventAmbig mp = mp
           relsFrom c = [Mph (name d) Nowhere [] (source d,target d) True d| d@(Sgn {})<-declarations context, source d == c]++
                        [flp (Mph (name d) Nowhere [] (source d,target d) True d)| d@(Sgn {})<-declarations context, target d == c]
           --TODO -> Explain all expressions (recursive) in the generated services
           --explained :: ObjectDef -> ObjectDef
           --explained obj@(Obj{objstrs=xs,objctx=e}) = obj{objstrs=["EXPLANATION: ", explain e]:xs} 
           recur :: [Morphism] -> Morphism -> ObjectDef
           recur ms m
            = Obj { objnm   = composedname m
                  , objpos  = Nowhere
                  , objctx  = Tm (preventAmbig m)
                  , objats  = (map head.eqCl objnm)
                                [ recur (ms++[mph]) mph
                                | mph<-relsFrom (target m), not (isSignal mph)
                                , isTot mph, not (isProperty mph)
                                , not (mph `elem` ms)]
                  , objstrs = [] -- [["DISPLAYTEXT", name m++" "++name (target m)]]++props (multiplicities m)
                  }
 
{- alleen nodig voor DISPLAYTEXT; herzien i.c.m. interface
           props ps = [if Sym `elem` ps && Asy `elem` ps then ["PROPERTY"] else
                       if Tot `elem` ps && Uni `elem` ps then ["ATTRIBUTE"] else
                       if Tot `elem` ps                  then ["NONEMPTY LIST"] else
                       if                  Uni `elem` ps then ["OPTIONAL FIELD"] else
                                                              ["LIST"]
                      ]
-}
        --TODO -> assign themerules to themes and remove them from the Anything theme
        themes' = FTheme{tconcept=Anything,tfunctions=[],trules=themerules}
                  :(map maketheme$orderby [(wsopertheme oper, oper)
                                          |oper<-themeoperations, wsopertheme oper /= Nothing])
        --TODO -> by default CRUD operations of datasets, possibly overruled by ECA or PHP plugs
        themeoperations = phpoperations++sqloperations
        phpoperations =[makeDSOperation$makePhpPlug phpplug | phpplug<-(ctxphp context)]
        sqloperations =[oper|obj<-ctxsql context, oper<-makeDSOperations (ctxks context) obj]
        --query copied from FSpec.hs revision 174
        themerules = [r|p<-patterns context, r<-rules p++signals p]
        maketheme (Just c,fs) = FTheme{tconcept=c,tfunctions=fs,trules=[]}
        maketheme _ = error("!Fatal (module ADL2Fspec 235): function makeFspec.maketheme: The theme must involve a concept.")
        orderby :: (Eq a) => [(a,b)] ->  [(a,[b])]
        orderby xs =  [(x,[y|(x',y)<-xs,x==x']) |x<-rd [dx|(dx,_)<-xs] ]

{- makePlugs computes a set of ECA plugs to obtain wide tables with minimal redundancy.
   First, we determine classes of concepts that are related by bijective relations.   Code:   eqClass bi (concs context)
   Secondly, we choose one concept as the kernel of that plug. Code:   c = minimum [g|g<-concs context,g<=head cl]
   Thirdly, we need all univalent relations that depart from this class to be the attributes. Code:   dss cl
   Then, all these morphisms are made into fields. Code: [mph2fld m | m<- mIs c: dss cs ]
   Now we have plugs. However, some are redundant. If there is a surjective relation from the kernel of plug p
   to the kernel of plug p' and there are no univalent relations departing from p',
   then plug p can serve as kernel for plug p'. Hence p' is redundant and can be removed. It is absorbed by p.
   So, we sort the plugs on length, the longest first. Code:   sort' ((0-).length.fields)
   Finally, we filter out all shorter plugs that can be represented by longer ones. Code: absorb
-}
   makePlugs :: Context -> [Plug] -> [Plug]
   makePlugs context currentPlugs
    = (absorb . sort' ((0-).length.fields))
       [ PlugSql { fields = [mph2fld m | m<- mIs c: dss cl ]
                 , plname = name c
                 , plfpa  = ILGV Eenvoudig
                 }
       | cl<-eqClass bi (concs'), c<-[minimum [g|g<-concs',g<=head cl]] ]
      where
       decls  = declarations context >- concat (map decsInPlug currentPlugs)
       concs' = concs context >- concat (map concsInPlug currentPlugs)
       decsInPlug  p = declarations (morsInPlug p)
       morsInPlug  p = [mor | Tm mor<-map fldexpr (fields p)]
       concsInPlug p = [source mor | mor <- morsInPlug p, isIdent mor]
       mph2fld m = Fld { fldname = name m
                       , fldexpr = Tm m
                       , fldtype = if isSQLId then SQLId else SQLVarchar 255
                       , fldnull = not (isTot m)          -- can there be empty field-values? 
                       , flduniq = isInj m                -- are all field-values unique?
                       , fldauto = isAuto                                     -- is the field auto increment?
                       } 
                   where isSQLId = isIdent m && isAuto
                         isAuto  = isIdent m
                                    && not (null [key| key<-ctxks context, target (kdctx key)==target m]) -- if there are any keys around, make this plug autoincrement.
                                    && null (contents m) -- and the the field may not contain any strings
       c `bi` c' = not (null [mph| mph<-decls, isFunction mph, isFunction (flp mph)
                                 , source mph<=c && target mph<=c'
                                   || source mph<=c' && target mph<=c])
{- The attributes of a plug are determined by the univalent relations that depart from the kernel. -}
       dss cl = [     makeMph d | d<-decls, isUni      d , source d `elem` cl]++
                [flp (makeMph d)| d<-decls, isUni (flp d), target d `elem` cl]

{- Absorb
If a concept is represented by plug p, and there is a surjective path between concept c' and c, then c' can be represented in the same table.
Hence, we do not need a separate plug for c' and it will be skipped.
-}
       absorb []     = []
       absorb (p:ps) = p: absorb [p'| p'<-ps
                                    , kernel p' `notElem` [target m| f<-fields p
                                                                   , Tm m<-[fldexpr f]
                                                                   , isSur m]]

       kernel :: Plug -> Concept -- determines the core concept of p. The plug serves as concept table for (kernel p).
       kernel p@(PlugSql{}) = source (fldexpr (head (fields p)))
       kernel _ = error("!Fatal (module ADL2Fspec 293): function \"kernel\"")


   makeSqlPlug :: ObjectDef -> Plug
   makeSqlPlug obj = PlugSql{fields=makeFields obj,plname=name obj,plfpa=ILGV Eenvoudig}
      where
      makeFields :: ObjectDef -> [SqlField]
      makeFields obj =
        [Fld{fldname = name att
            ,fldexpr = objctx att
            ,fldtype = sqltp att
            ,fldnull = nul att
            ,flduniq = uniq att
            ,fldauto = att `elem` autoFields
            }
        | att<-objats obj
        ]
        where nul  att = not (isTot (objctx att))
              uniq att = null [a' | a' <- objats obj
                                  , not (isUni (disjNF$F[flp$objctx att,objctx a']))]
              autoFields = take 1 [a'| a'<-objats obj
                                     , sqltp a'==SQLId, isTot (objctx a')
                                     , uniq a', isIdent $ objctx a' ]
      sqltp :: ObjectDef -> SqlType
      sqltp obj = head $ [makeSqltype sqltp' | strs<-objstrs obj,('S':'Q':'L':'T':'Y':'P':'E':'=':sqltp')<-strs]
                         ++[SQLVarchar 255]
      makeSqltype :: String -> SqlType
      makeSqltype str = case str of
          ('V':'a':'r':'c':'h':'a':'r':_) -> SQLVarchar 255 --TODO number
          ('P':'a':'s':'s':_) -> SQLPass
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
                             ,phpfile="phpPlugs.inc.php",plname=name plug,plfpa=KGV Eenvoudig}
      where
      makeActiontype = head $ [case str of {"SELECT"->Read;
                                            "CREATE"->Create;
                                            "UPDATE"->Update;
                                            "DELETE"->Delete;
                                            _ -> error $ "!Fatal (module ADL2Fspec 341): Choose from ACTION=[SELECT|CREATE|UPDATE|DELETE].\n"  
                                                         ++ show (objpos plug)
                                           }
                     | strs<-objstrs plug,'A':'C':'T':'I':'O':'N':'=':str<-strs]
                     ++ [error $ "!Fatal (module ADL2Fspec 345): Specify ACTION=[SELECT|CREATE|UPDATE|DELETE] on phpplug.\n"  ++ show (objpos plug)]
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
                                            _ -> error $ "!Fatal (module ADL2Fspec 356): Choose from PHPTYPE=[String|Int|Float|Array].\n"  
                                                        ++ show (objpos objat)
                                           }
                     | strs<-objstrs objat,'P':'H':'P':'T':'Y':'P':'E':'=':str<-strs]
                     ++ [error $ "!Fatal (module ADL2Fspec 360): Specify PHPTYPE=[String|Int|Float|Array] on PHPARG or PHPRETURN.\n"
                                 ++ show (objpos objat)]

   --DESCR -> Use for plugs that describe a single operation like PHP plugs
   makeDSOperation :: Plug -> WSOperation
   makeDSOperation PlugSql{} = error $ "!Fatal (module ADL2Fspec 365): function makeDSOperation: ECA plugs do not describe a single operation."
   makeDSOperation p@PlugPhp{} = 
       let nullval val = case val of
                         PhpNull    -> True
                         PhpObject{}-> False
           towsaction x = case x of {Create->WSCreate;Read->WSRead;Update->WSUpdate;Delete->WSDelete}
       in WSOper{wsaction=towsaction$action$function p
                 ,wsmsgin=[objectdf arg|(_,arg)<-args p,nullval$arg]
                 ,wsmsgout=[objectdf$retval$returns p|nullval$retval$returns p]
                 }
   --DESCR -> Use for objectdefs that describe all four CRUD operations like ECA plugs
   makeDSOperations :: [KeyDef] -> ObjectDef -> [WSOperation]
   makeDSOperations kds obj = 
       [WSOper{wsaction=WSCreate,wsmsgin=[obj],wsmsgout=[]}
       ,WSOper{wsaction=WSUpdate,wsmsgin=[obj],wsmsgout=[]}
       ,WSOper{wsaction=WSDelete,wsmsgin=[obj],wsmsgout=[]}]
     ++(if null keydefs then [readby []] else map readby keydefs)
       where
       keydefs = [kdats kd|kd<-kds,objtheme obj==keytheme kd]
       readby keyobj = WSOper{wsaction=WSRead,wsmsgin=keyobj,wsmsgout=[obj]}
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

   --DESCR -> returns the concept on which the keydef is defined 
   keytheme :: KeyDef -> Concept
   keytheme kd = source$kdctx kd

   editable :: Expression -> Bool
   editable (Tm Mph{})  = True
   editable (Tm I{})    = True
   editable _           = False

   editMph :: Expression -> Morphism
   editMph (Tm m@Mph{}) = m
   editMph (Tm m@I{})   = m
   editMph e            = error("!Fatal (module ADL2Fspec 417): cannot determine an editable declaration in a composite expression: "++show e)

   makeFservice :: Context -> [Quad] -> ObjectDef -> Fservice
   makeFservice context allQuads object
    = let s = Fservice{ fsv_objectdef = object  -- the object from which the service is drawn
-- The declarations that may be changed by the user of this service are represented by fsv_rels
                      , fsv_rels      = rels
-- The rules that may be affected by this service
                      , fsv_rules     = invariants
                      , fsv_quads     = qs
-- The ECA-rules that may be used by this service to restore invariants.
                      , fsv_ecaRules  = nECArules
-- All signals that are visible in this service
                      , fsv_signals   = []
-- All fields/parameters of this service
                      , fsv_fields    = fields
-- All concepts of which this service can create new instances
                      , fsv_creating  = [c| c<-rd (map target rels), t<-fsv_ecaRules s, ecaTriggr (t arg)==On Ins (mIs c)]
-- All concepts of which this service can delete instances
                      , fsv_deleting  = [c| c<-rd (map target rels), t<-fsv_ecaRules s, ecaTriggr (t arg)==On Del (mIs c)]
                      , fsv_fpa       = case depth object of -- Valideren in de FPA-wereld
                                          0 -> NO
                                          1 -> IF Eenvoudig
                                          2 -> IF Eenvoudig
                                          3 -> IF Gemiddeld
                                          _ -> IF Moeilijk 
                      } in s
    where
        rels = rd (recur object)
         where recur obj = [editMph (objctx o)| o<-objats obj, editable (objctx o)]++[m| o<-objats obj, m<-recur o]
        depth :: ObjectDef -> Int
        depth obj  = foldr max 0 [depth o| o<-objats obj]+1
        vis        = rd (map makeInline rels++map (mIs.target) rels)
        visible m  = makeInline m `elem` vis
        invariants = [rule| rule<-rules context, not (null (map makeInline (mors rule) `isc` vis))]
        qs         = [q| q@(Quad m ccrs)<-allQuads, m `elem` vis
                       , (_,shifts)<-cl_conjNF ccrs
                       , Fu fus<-shifts, f<-fus, isPos f, m'<-mors f, m' `elem` vis]
        ecaRs      = assembleECAs visible qs
        nECArules  = map normECA ecaRs
        trigs :: ObjectDef -> [Declaration->ECArule]
        trigs obj  = [c | editable (objctx obj), c<-nECArules {- ,not (isBlk (ecaAction (c arg))), not (isDry (ecaAction (c arg))) -} ]
        arg = error("!Todo (module ADL2Fspec 463): declaratie Delta invullen")
        fields = [fld 0 o| o<-objats object]
        fld :: Int -> ObjectDef -> Field
        fld sLevel obj
         = Att { fld_name     = objnm obj
               , fld_sub      = [fld (sLevel +1) o| o<-objats obj]
               , fld_expr     = objctx obj
               , fld_mph      = if editable (objctx obj)
                                then editMph (objctx obj)
                                else error("!Fatal (module ADL2Fspec 461): cannot edit a composite expression: "++show (objctx obj)++"\nPlease test editability of field "++objnm obj++" by means of fld_editable first!")
               , fld_editable = editable (objctx obj)      -- can this field be changed by the user of this service?
               , fld_list     = not (isUni (objctx obj))   -- can there be multiple values in this field?
               , fld_must     = isTot (objctx obj)         -- is this field obligatory?
               , fld_new      = True                       -- can new elements be filled in? (if no, only existing elements can be selected)
               , fld_sLevel   = sLevel                     -- The (recursive) depth of the current servlet wrt the entire service. This is used for documentation.
               , fld_insAble  = not (null insTrgs)         -- can the user insert in this field?
               , fld_onIns    = case insTrgs of
                                 []  ->  error("!Fatal (module ADL2Fspec 469): no insert functionality found in field "++objnm obj++" of service "++name obj++" on line: "++show (pos (objctx obj)))
                                 [t] ->  t
                                 _   ->  error("!Fatal (module ADL2Fspec 471): multiple insert triggers found in field "++objnm obj++" of service "++name obj++" on line: "++show (pos (objctx obj)))
               , fld_delAble  = not (null delTrgs)         -- can the user delete this field?
               , fld_onDel    = case delTrgs of
                                 []  ->  error("!Fatal (module ADL2Fspec 474): no delete functionality found in field "++objnm obj++" of service "++name obj++" on line: "++show (pos (objctx obj)))
                                 [t] ->  t
                                 _   ->  error("!Fatal (module ADL2Fspec 476): multiple delete triggers found in field "++objnm obj++" of service "++name obj++" on line: "++show (pos (objctx obj)))
               }
           where triggers = trigs obj
                 insTrgs  = [c | c<-triggers, ecaTriggr (c arg)==On Ins (makeInline (editMph (objctx obj))) ]
                 delTrgs  = [c | c<-triggers, ecaTriggr (c arg)==On Del (makeInline (editMph (objctx obj))) ]

-- Comment on fld_new:
-- Consider this: New elements cannot be filled in
--    if there is a total relation r with type obj==source r  (i.e. r comes from obj),
--    which is outside the scope of this service.
-- Why? If you were to insert a new obj, x, then r would require a new link (x,y).
--    However, since r is out of scope, you cannot insert (x,y) into r.
-- More generally, if there is an ECA rule with I[type obj] in its left hand side,
--    and a right hand side that is out of scope of this service,
--    you may not insert a new element in obj.

   makeFSid1 :: String -> FSid
   makeFSid1 s = FS_id (firstCaps s)  -- We willen geen spaties in de naamgeveing.


--   fst3 :: (a,b,c) -> a
--   fst3 (a,_,_) = a
--   snd3 :: (a,b,c) -> b
--   snd3 (_,b,_) = b


