{-# OPTIONS_GHC -Wall #-}
module ADL2Fspec (makeFspec,actSem, delta, allClauses, conjuncts, quads, assembleECAs, preEmpt, doCode)
  where
   import Collection     (Collection(rd,rd',uni,isc,(>-)))
   import CommonClasses  (ABoolAlg(..))
   import Adl
--   import Strings
   import Auxiliaries    (eqCl, eqClass, sort')
   import Data.Fspec
   import Options        (Options)
   import NormalForms(conjNF,disjNF,normPA,simplify)
   import Data.Plug
   import Char
   import ShowADL
--   import ShowHS
   import FPA
   
   makeFspec :: Options -> Context -> Fspc
   makeFspec _ context = fSpec
    where
        allQuads = quads (\_->True) (rules context++multrules context)
        fSpec =
            Fspc { fsName       = name context
                   -- serviceS contains the services defined in the ADL-script.
                   -- services are meant to create user interfaces, programming interfaces and messaging interfaces.
                   -- A generic user interface (the Lonneker interface) is already available.
                 , vplugs       = definedplugs
                 , plugs        = allplugs
                 , serviceS     = attributes context -- services specified in the ADL script
                 , serviceG     = [ o| o<-serviceGen, not (objctx o `elem` map objctx (serviceS fSpec))]   -- generated services
                 , services     = [ makeFservice context allQuads a | a <-serviceS fSpec++serviceG fSpec]
                 , vrules       = rules context++signals context
                 , vconjs       = rd [conj| Quad _ ccrs<-allQuads, (conj,_)<-cl_conjNF ccrs]
                 , vquads       = allQuads
--                 , ecaRules     = []
                 , vrels        = allDecs
                 , fsisa        = ctxisa context
                 , vpatterns    = patterns context
                 , vgens        = gens context
                 , vkeys        = keyDefs context
                 , pictPatts    = [] --Nothing
                 , vConceptDefs = conceptDefs context
                 , themes       = themes'
                 , fSexpls      = fSexpls'
                 , vctxenv      = ctxenv context
                 }
        allDecs = [ d{decprps_calc = multiplicities d `uni` [Tot|m<-totals, d==makeDeclaration m, inline m]
                                                      `uni` [Sur|m<-totals, d==makeDeclaration m, not (inline m)]}
                  | d<-declarations context, deciss d || decusr d
                  ]
        definedplugs = vsqlplugs ++ vphpplugs

-- mor2plug creates associations between plugs that represent wide tables.
-- this concerns relations that are not univalent nor injective,
-- Univalent relations and injective relations cannot be associations, because they are used as attributes in wide tables.
        mor2plug :: Morphism -> Plug
        mor2plug  m
         = if Inj `elem` mults || Uni `elem` mults then error ("!Fatal (module ADL2Fspec 64): unexpected call of mor2plug("++show m++"), because it is injective or univalent.") else
           if not is_Tot && is_Sur then mor2plug (flp m) else
           PlugSql (name m)                                                    -- plname
                   [srcFld,trgFld]                                             -- fields
                   ([(source m,srcFld)| is_Tot]++[(target m,trgFld)| is_Sur])  -- cLkpTbl
                   [(m,srcFld,trgFld)]                                         -- mLkpTbl 
                   NO                                                          -- plfpa 
           where
             srcNm = (if name (source m)==name (target m) then "s_" else "")++name (source m)
             trgNm = (if name (source m)==name (target m) then "t_" else "")++name (target m)
             srcFld = field srcNm
                            (if   is_Tot
                             then Tm (mIs (source m)) (-1)                                    -- DAAROM (SJ) Deze relatie mag als conceptentabel voor source m worden gebruikt, omdat ze totaal is.
                             else Fix [Tm (mIs (source m))(-1),F [Tm m(-1),flp (Tm m(-1))]]    -- DAAROM (SJ) Deze expressie, nl. I/\m;m~,  representeert het domein van deze relatie. Dat is nodig, omdat er andere elementen in I kunnen zitten, die niet in het domein van m voorkomen. m kan dus niet als conceptentabel worden gebruikt.
                            )
                            Nothing
                            (not is_Sur)
                            (isUni m {- will be False -})
             trgFld = field trgNm
                            (Tm m (-1))
                            Nothing
                            (not is_Tot)
                            (isInj m {- will be False -})
             mults = multiplicities m
             is_Tot = Tot `elem` mults || m `elem` totals
             is_Sur = Sur `elem` mults || flp m `elem` totals
        totals :: Morphisms
        totals
         = rd [ m | q<-quads visible (rules fSpec), isIdent (qMorph q)
                  , (_,hcs)<-cl_conjNF (qClauses q), Fux fus<-hcs
                  , antc<-[(conjNF.Fix) [notCp f| f<-fus, isNeg f]], isIdent antc
                  , f<-fus, isPos f
                  , m<-tots f
                  ]
           where tots (F fs) = [m| Tm m _<-take 1 fs]++[flp m| Tm m _<-take 1 (reverse fs)]
                 tots _ = []
                 visible _ = True -- for computing totality, we take all quads into account.

        allplugs = definedplugs ++      -- all plugs defined by the user
                   uniqueNames definedplugs  -- the names of definedplugs will not be changed, assuming they are all unique
                    (gPlugs       ++      -- all plugs generated by the compiler
                     relPlugs             -- all plugs for relations not touched by definedplugs and gplugs
                    )
          where
           gPlugs   = makePlugs context allDecs definedplugs
           relPlugs = [ mor2plug (makeMph d)
                      | d<-allDecs
                      , not (Inj `elem` multiplicities d)
                      , not (Uni `elem` multiplicities d)]

        uniqueNames :: [Plug]->[Plug]->[Plug]
        -- Some target systems may be case insensitive! For example MySQL.
        -- So, unique names are made in a case insensitive manner.
        uniqueNames taken plgs
         = [p | cl<-eqCl (map toLower.name) plgs  -- each equivalence class cl contains plugs p with the same map toLower (name p)
              , p <-if name (head cl) `elem` map name taken || length cl>1
                    then [p{plname=name p++show i}| (p,i)<-zip cl [(1::Int)..]]
                    else cl
           ]
{- replaces the following code, because the new code is easier to understand
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
-}
        vsqlplugs = map makeSqlPlug (ctxsql context)
        vphpplugs = map makePhpPlug (ctxphp context)
        -- services (type ObjectDef) can be generated from a basic ontology. That is: they can be derived from a set
        -- of relations together with multiplicity constraints. That is what serviceG does.
        -- This is meant to help a developer to build his own list of services, by providing a set of services that works.
        -- The developer will want to assign his own labels and maybe add or rearrange attributes.
        -- This is easier than to invent a set of services from scratch.

        -- Rule: a service must be large enough to allow the required transactions to take place within that service.
        -- Attributen van elk object hebben unieke namen.

--- generation of services:
--  Step 1: select and arrange all declarations to obtain a set cRels of total relations
--          to ensure insertability of entities
        cRels = [     morph d | d<-declarations context, decusr d, isTot d]++
                [flp (morph d)| d<-declarations context, decusr d, not (isTot d) && isSur d]
--  Step 2: select and arrange all declarations to obtain a set cRels of injective relations
--          to ensure deletability of entities
        dRels = [     morph d | d<-declarations context, decusr d, isInj d]++
                [flp (morph d)| d<-declarations context, decusr d, not (isInj d) && isUni d]
--  Step 3: compute maximally total expressions and maximally injective expressions.
--  DAAROM
--   (GMI): Moet voor een concept dat 'los staat' (geen attribuut van, en heeft zelf geen attributen)
--          geen service genereerd worden? => SERVICE Concept: I[Concept]
--          VOORBEELD: PATTERN x r::A*B. s::B*C. t::A*C. ENDPATTERN geen multipliciteiten=>serviceGen=[]

        maxTotExprs = clos cRels
        maxInjExprs = clos dRels
--  Step 4: generate services from the maximally total expressions and maximally injective expressions.
        serviceGen
         = [ Obj (name c)         -- objnm
                 Nowhere          -- objpos
                 (Tm (mIs c)(-1)) -- objctx
                 Nothing          -- objctx_proof
                 objattributes    -- objats
                 []               -- objstrs
           | cl <- eqCl source (maxTotExprs `uni` maxInjExprs)
           , let objattributes = recur [] cl
           , not (null objattributes)
           , let e0=head cl, let c=source e0
           , map toLower (name c) `notElem` map (map toLower.name) scalarPlugs       -- exclude scalars
           ]
        scalarPlugs = [p|p<-allplugs, isScalar p]
--  Auxiliaries for generating services:
        morph d = Mph (name d) (pos d) [] (source d,target d) True d
--    Warshall's transitive closure algorithm, adapted for this purpose:
        clos :: Morphisms -> Expressions
        clos xs
         = f [F [Tm x (-1)]| x<-xs] (rd (map source xs) `isc` rd (map target xs))
           where
            f q (x:xs') = f (q ++ [F (ls++rs)| l@(F ls)<-q, x<=target l
                                             , r@(F rs)<-q, x<=source r
                                             , null (ls `isc` rs)
                                             ]) xs'
            f q []      = q
       
        recur trace es
         = [ Obj (showADLcode fSpec t)     -- objnm
                 Nowhere                   -- objpos
                 t                         -- objctx
                 Nothing                   -- objctx_proof
                 (recur (trace++[c]) cl)   -- objats
                 []                        -- objstrs
           | cl<-eqCl (\(F ts)->head ts) es, F ts<-take 1 cl, t<-[head ts], c<-[source t], c `notElem` trace ]
        --TODO -> assign themerules to themes and remove them from the Anything theme
        themes' = FTheme{tconcept=Anything,tfunctions=[],trules=themerules}
                  :(map maketheme$orderby [(wsopertheme oper, oper)
                                          |oper<-themeoperations, wsopertheme oper /= Nothing])
        fSexpls' = explanations context                ++
                   explanations (patterns context)     ++
                   explanations (rules context)
        --TODO -> by default CRUD operations of datasets, possibly overruled by ECA or PHP plugs
        themeoperations = phpoperations++sqloperations
        phpoperations =[makeDSOperation$makePhpPlug phpplug | phpplug<-(ctxphp context)]
        sqloperations =[oper|obj<-ctxsql context, oper<-makeDSOperations (vkeys fSpec) obj]
        --query copied from FSpec.hs revision 174
        themerules = [r|p<-patterns context, r<-rules p++signals p]
        maketheme (Just c,fs) = FTheme{tconcept=c,tfunctions=fs,trules=[]}
        maketheme _ = error("!Fatal (module ADL2Fspec 235): function makeFspec.maketheme: The theme must involve a concept.")
        orderby :: (Eq a) => [(a,b)] ->  [(a,[b])]
        orderby xs =  [(x,[y|(x',y)<-xs,x==x']) |x<-rd [dx|(dx,_)<-xs] ]

{- makePlugs computes a set of plugs to obtain wide tables with little redundancy.
   First, we determine the kernels for all plugs.
   A kernel is a set of univalent, injective, and surjective relations, with one root concept.
   The root of a kernel is the concept that is either the source of a relation in the kernel, or that relation is reachable from the source by a surjective path.
   Code: kernels represents the relations of the plug (which are all univalent, injective, and surjective)
         target (head kernel) represents the root concept of the plug
   Secondly, we take all univalent relations that are not in the kernel, but depart from this kernel.
   These relations serve as attributes. Code:  [a| a<-attMors, source a `elem` concs kernel]
   Then, all these morphisms are made into fields. Code: plugFields = [mph2fld plugMors a| a<-plugMors]
   We also define two lookup tables, one for the concepts that are stored in the kernel, and one for the attributes of these concepts.
   For the fun of it, we sort the plugs on length, the longest first. Code:   sort' ((0-).length.fields)
   By the way, parameter allDecs contains all relations that are declared in context, enriched with extra multiplicities.
   This parameter was added to makePlugs to avoid recomputation of the extra multiplicities.
-}
   makePlugs :: Context -> Declarations -> [Plug] -> [Plug]
   makePlugs context allDecs currentPlugs
    = sort' ((0-).length.fields)
       [ PlugSql (name c)               -- plname
                 plugFields             -- fields
                 conceptLookuptable     -- cLkpTbl
                 attributeLookuptable   -- mLkpTbl
                 (ILGV Eenvoudig)       -- plfpa
       | kernel<-kernels
       , let c = target (head kernel)               -- one concept from the kernel is designated to "lead" this plug.
             plugMors              = kernel++[a| a<-attMors, source a `elem` concs kernel]
             plugFields            = [fld a| a<-plugMors]      -- Each field comes from a relation.
             conceptLookuptable   :: [(Concept,SqlField)]
             conceptLookuptable    = [(target m,fld m)|cl<-eqCl target kernel, let m=head cl]
             attributeLookuptable :: [(Morphism,SqlField,SqlField)]
             attributeLookuptable  = [(m,lookupC (source m),fld m)| m<-plugMors] -- kernel attributes are always surjective from left to right. So do not flip the lookup table!
             lookupC cpt           = head [f|(c',f)<-conceptLookuptable, cpt==c']
             fld                   = mph2fld plugMors
       ]
      where
-- The first step is to determine which plugs to generate. All concepts and declarations that are used in plugs in the ADL-script are excluded from the process.
       nonCurrDecls = [d| d@Sgn{}<-allDecs >- concat (map decls currentPlugs), decusr d]
-- For making kernels as large as possible, the univalent and injective declarations will be flipped if that makes them surjective.
-- kernelMors contains all relations that occur in kernels.
-- note that kernelMors contains no I-relations, because all declarations from nonCurrDecls match @Sgn{}.
       kernelMors   = [m|m<-ms, isSur m]++[flp m|m<-ms, not (isSur m), isTot m]
                      where ms = [makeMph d| d<-nonCurrDecls, isUni d, isInj d]
-- iniKernels contains the set of kernels that would arise if kernelMors were empty. From that starting point, the kernels are computed recursively in code that follows (kernels).
       iniKernels   = [(c,[])| c<-concs context, c `notElem` map concept currentPlugs]
       attMors      = [     makeMph d  | d<-nonCurrDecls, isUni d, not (d `elem` decls kernelMors)]++
                      [flp (makeMph d) | d<-nonCurrDecls, not (isUni d), isInj d, not (d `elem` decls kernelMors)]
{- The second step is to make kernels for all plugs. In principle, every concept would yield one plug.
However, if two concepts are mutually connected through a surjective, univalent and injective relation, they are combined in one plug.
So the first step is create the kernels ...   -}
       kernels :: [[Morphism]]
       kernels
        = --error ("Diag ADL2Fspec "++show (kernelMors)++"\n"++show (map fst iniKernels)++"\n"++show (expand iniKernels))++
          [ mIs c: ms               -- one morphism for each concept in the kernel
          | (c,ms)<-f iniKernels    -- the initial kernels
          ]
          where
            f :: [(Concept,[Morphism])] -> [(Concept,[Morphism])]
            f ks = if ks==nks then merge (reverse ks) else f (merge nks)      -- all r<-kernelMors are surjective, univalent and injective
             where nks = expand ks
            expand ks = [(c, ms++[r|r<-kernelMors, r `notElem` ms, source r `elem` c:concs ms])| (c,ms)<-ks] -- expand a kernel (c,ms) by one step
            merge ks = if nks==ks then ks else merge nks
             where nks = oneRun ks
                   oneRun [] = []
                   oneRun ((c,ms):ks') = (c, ms++[m|(c',ms')<-ks', c' `elem` c:concs ms, m<-ms', m `notElem` ms]):
                                         oneRun [k|k@(c',_)<-ks', c' `notElem` c:concs ms]
       {- Kernels are built recursively. Kernels expand by adding (sur, uni and inj) relations until there are none left.
          Step 1: compute the expansion of each kernel (code: ms++[r|r<-rs, source r `elem` concs ms])
          Step 2: merge kernels if possible (code: recursion over oneRun)
          Step 3: compute the remaining relations (code: [r| r<-rs, source r `notElem` concs [ms| (_,ms)<-kernels]] )
          And call recursively until there are none left. -}

-- Each morphism yields one field in the plug...
-- The parameter ms defines the name space, making sure that all fields within a plug have unique names.
       mph2fld :: [Morphism] -> Morphism -> SqlField
       mph2fld ms m
        = Fld fldName                                      -- fldname : 
              (Tm m (-1))                                  -- fldexpr : De target van de expressie geeft de waarden weer in de SQL-tabel-kolom.
              (if isSQLId then SQLId else SQLVarchar 255)  -- fldtype :
              (not (isTot m))                              -- fldnull : can there be empty field-values? 
              (isInj m)                                    -- flduniq : are all field-values unique?
              isAuto                                       -- fldauto : is the field auto increment?
          where fldName = head [nm| (m',nm)<-table, m==m']
                isSQLId = isIdent m && isAuto
                isAuto  = isIdent m
                           && not (null [key| key<-keyDefs context, kdcpt key==target m]) -- if there are any keys around, make this plug autoincrement.
                           && null (contents m) -- and the the field may not contain any strings
                table   = [ entry
                          | cl<-eqCl (map toLower.name) ms
                          , entry<-if length cl==1 then [(r,name r)|r<-cl] else tbl cl]
                tbl rs  = [ entry
                          | cl<-eqCl (map toLower.name.source) rs
                          , entry<-if length cl==1 then [(r,name r++name (source r))|r<-cl] else [(r,name r++show i)|(r,i)<-zip cl [(0::Int)..]]]

-- makeSqlPlug is used to make user defined plugs. One advantage is that the field names can be controlled by the user. 
   makeSqlPlug :: ObjectDef -> Plug
   makeSqlPlug obj = PlugSql (name obj)             -- plname
                             makeFields             -- fields
                             cptflds                -- cLkpTbl -- TODO: invullen!
                             mphflds                -- mLkpTbl -- TODO: invullen!
                             (ILGV Eenvoudig)       -- plfpa
      where
      --TODO: cptflds and mphflds assume that the user defined plug is a concept plug: 
      --      -> containing just one expression equivalent to the identity relation
      --      -> if the expression is not the identity relation then it is a simple expression (a morphism)
      --      if there are more expressions equivalent to the identity relation then a fatal
      --      the others would probably be ignored by updates and inserts, but fldnull=false => save errors in SQL?
      cptflds = (\xs->if length xs==1 then xs else error "!Fatal (module ADL2Fspec 319): Implementation expects only one identity relation in plug.")
                [(source (fldexpr f),f)|f<-makeFields, isIdent(fldexpr f)]
      mphflds = [(m,cptfld,f)|f<-makeFields, length (mors(fldexpr f))==1,m@(Mph{})<-mors(fldexpr f), let (_,cptfld)=head cptflds]
      makeFields ::  [SqlField]
      makeFields =
        [Fld (name att)                 -- fldname : 
             (objctx att)               -- fldexpr : De target van de expressie geeft de waarden weer in de SQL-tabel-kolom.
             (sqltp att)                -- fldtype :
             (not (isTot (objctx att))) -- fldnull : can there be empty field-values? 
             (isInj (objctx att))       -- flduniq : are all field-values unique?
             (att `elem` autoFields)    -- fldauto : is the field auto increment?
        | att<-objats obj
        ]
        where autoFields = take 1 [a'| a'<-objats obj
                                     , sqltp a'==SQLId, isTot (objctx a')
                                     , isInj (objctx a'), isIdent (objctx a')]
      sqltp :: ObjectDef -> SqlType
      sqltp att = head $ [makeSqltype sqltp' | strs<-objstrs att,('S':'Q':'L':'T':'Y':'P':'E':'=':sqltp')<-strs]
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
                 | (i,oa)<-zip [(1::Int)..] (objats plug), strs<-(objstrs oa), elem "PHPARG" strs]
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
   keytheme kd = kdcpt kd

   editable :: Expression -> Bool   --TODO deze functie staat ook in Calc.hs...
   editable (Tm Mph{} _)  = True
   editable (Tm I{} _)    = True
   editable _           = False

   editMph :: Expression -> Morphism  --TODO deze functie staat ook in Calc.hs...
   editMph (Tm m@Mph{} _) = m
   editMph (Tm m@I{} _)   = m
   editMph e            = error("!Fatal (module ADL2Fspec 425): cannot determine an editable declaration in a composite expression: "++show e)

   makeFservice :: Context -> [Quad] -> ObjectDef -> Fservice
   makeFservice context _ object
    = let s = Fservice{ fsv_objectdef = object  -- the object from which the service is drawn
-- The relations that may be edited by the user of this service are represented by fsv_insrels and fsv_delrels.
-- Editing means that tuples can be added to or removed from the population of the relation.
-- The relations in which the user may insert elements:
                      , fsv_insrels   = map makeInline rels>-[makeInline m|er<-ecaRs, On Ins m<-[ecaTriggr er], Blk _<-[ecaAction er]]
-- The relations from which the user may remove elements:
                      , fsv_delrels   = map makeInline rels>-[makeInline m|er<-ecaRs, On Del m<-[ecaTriggr er], Blk _<-[ecaAction er]]
-- The rules that may be affected by this service
                      , fsv_rules     = invariants
                      , fsv_quads     = qs
-- The ECA-rules that may be used by this service to restore invariants. TODO: de Delta-parameter is nog fout!
                      , fsv_ecaRules  = [\_->er{ecaAction = action'}| er<-ecaRs, let action'=ecaAction er]
-- All signals that are visible in this service
                      , fsv_signals   = [sig|sig<-signals context]
-- All fields/parameters of this service
                      , fsv_fields    = srvfields
-- All concepts of which this service can create new instances
                      , fsv_creating  = [c| c<-rd (map target (fsv_insrels s)), t<-fsv_ecaRules s, ecaTriggr (t arg)==On Ins (mIs c)]
-- All concepts of which this service can delete instances
                      , fsv_deleting  = [c| c<-rd (map target (fsv_delrels s)), t<-fsv_ecaRules s, ecaTriggr (t arg)==On Del (mIs c)]
                      , fsv_fpa       = case depth object of -- Valideren in de FPA-wereld
                                          0 -> NO
                                          1 -> IF Eenvoudig
                                          2 -> IF Eenvoudig
                                          3 -> IF Gemiddeld
                                          _ -> IF Moeilijk 
                      , fsv_expls     = [] -- TODO: alle explanations verzamelen van dingen die in de service zitten
                      } in s
    where
-- step 1: the relations that yield potentially editable fields. These relations are called "visible".
        rels = rd (recur object)
         where recur obj = [editMph (objctx o)| o<-objats obj, editable (objctx o)]++[m| o<-objats obj, m<-recur o]
        vis         = rd (map makeInline rels++map (mIs.target) rels)
        visible r   = makeInline r `elem` vis
-- step 2: the rules that must be maintained automatically, and can possibly be affected by a transaction in this service
--         If none of the relations in a rule are visible, exclude that rule...
        invariants  = [rule| rule<-rules context,  not (null (map makeInline (mors rule) `isc` vis))]
-- step 3: the quads that can be derived from these rules, considering which relations are visible.
--         (A quad contains the conjunct(s) to be maintained.)
        qs          = quads visible invariants
-- step 4: the ECA rules derived from the quads. Cascaded blocking rules are preempted to save some excess code.
        ecaRs       = preEmpt (assembleECAs visible qs)
-- step 5: signalInvs contains the rules that might possibly be maintained by the user, while performing a transaction in this service.
--         If none of the relations in a rule are visible, exclude the rule...
--        signalInvs  = [rule| rule<-signals context, not (null (map makeInline (mors rule) `isc` vis))]
-- step 6: ECA rules derived from the signalInvs. These rules may be used to suggest ways to the user to restore signals.
--        signalRs    = preEmpt (assembleECAs visible (quads visible signalInvs))
        depth :: ObjectDef -> Int
        depth obj   = foldr max 0 [depth o| o<-objats obj]+1
        trigs :: ObjectDef -> [Declaration->ECArule]
        trigs _  = [] -- [c | editable (objctx obj), c<-nECArules {- ,not (isBlk (ecaAction (c arg))) -} ]
        arg = error("!Todo (module ADL2Fspec 467): declaratie Delta invullen")
        srvfields = [fld 0 o| o<-objats object]
        fld :: Int -> ObjectDef -> Field
        fld sLevel obj
         = Att { fld_name     = objnm obj
               , fld_sub      = [fld (sLevel +1) o| o<-objats obj]
               , fld_expr     = objctx obj
               , fld_mph      = if editable (objctx obj)
                                then editMph (objctx obj)
                                else error("!Fatal (module ADL2Fspec 476): cannot edit a composite expression: "++show (objctx obj)++"\nPlease test editability of field "++objnm obj++" by means of fld_editable first!")
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


--   fst3 :: (a,b,c) -> a
--   fst3 (a,_,_) = a
--   snd3 :: (a,b,c) -> b
--   snd3 (_,b,_) = b

   -- Quads embody the "switchboard" of rules. A quad represents a "proto-rule" with the following meaning:
   -- whenever Morphism m is affected (i.e. tuples in m are inserted or deleted),
   -- the rule may have to be restored using functionality from one of the clauses.
   -- The rule is carried along for traceability.
   quads :: (Morphism->Bool) -> Rules  -> [Quad]
   quads visible rs
    = [ Quad m (Clauses [ (conj,allShifts conj)
                        | conj <- conjuncts rule
      --                , (not.null.lambda Ins (Tm m)) conj  -- causes infinite loop
      --                , not (checkMono conj Ins m)         -- causes infinite loop
                        , let conj' = subst (m, actSem Ins m (delta (sign m))) conj
                        , (not.isTrue.conjNF) (Fux[Cpx conj,conj']) -- the system must act to restore invariance     
                        ]
                        rule)
      | rule<-rs
      , m<-rd (map makeInline (mors rule))
      , visible m
      ]

-- The function allClauses yields an expression which has constructor Fu in every case.
   allClauses :: Rule -> Clauses
   allClauses rule = Clauses [(conj,allShifts conj)| conj<-conjuncts rule] rule

   allShifts :: Expression -> Expressions
   allShifts conjunct = rd [simplify (normFlp e')| e'<-shiftL conjunct++shiftR conjunct, not (isTrue e')]
    where
       normFlp (Fux []) = Fux []
       normFlp (Fux fs) = if length [m| f<-fs, m<-morlist f, inline m] <= length [m| f<-fs, m<-morlist f, not (inline m)]
                         then Fux (map flp fs) else (Fux fs)
       normFlp _ = error ("!Fatal (module Calc 61): normFlp must be applied to Fu expressions only, look for mistakes in shiftL or shiftR")

   shiftL :: Expression -> Expressions
   shiftL r
    | length antss+length conss /= length fus = error ("!Fatal (module Calc 65): shiftL will not handle argument of the form "++showADL r)
    | null antss || null conss                = [disjuncts r|not (null fs)] --  shiftL doesn't work here.
    | idsOnly antss                           = [Fux ([Cpx (F [Tm (mIs srcA)(-1)])]++map F conss)]
    | otherwise                               = [Fux ([ Cpx (F (if null ts then id' css else ts))
                                                     | ts<-ass++if null ass then [id' css] else []]++
                                                     [ F (if null ts then id' ass else ts)
                                                     | ts<-css++if null css then [id' ass] else []])
                                                | (ass,css)<-rd(move antss conss)
                                                , if null css then error "!Fatal (module Calc 73): null css in shiftL" else True
                                                , if null ass then error "!Fatal (module Calc 74): null ass in shiftL" else True
                                                ]
    where
     Fux fs = disjuncts r
     fus = filter (not.isIdent) fs
     antss = [ts | Cpx (F ts)<-fus]
     conss = [ts | F ts<-fus]
     srcA = -- if null antss  then error ("!Fatal (module Calc 81): empty antecedent in shiftL ("++showHS options "" r++")") else
            if length (eqClass order [ source (head ants) | ants<-antss])>1 then error ("!Fatal (module Calc 82): shiftL ("++showADL r++")\nin calculation of srcA\n"++show (eqClass order [ source (head ants) | ants<-antss])) else
            foldr1 lub [ source (head ants) | ants<-antss]
     id' ass = [Tm (mIs c) (-1)]
      where a = (source.head.head) ass
            c = if not (a `order` b) then error ("!Fatal (module Calc 86): shiftL ("++showADL r++")\nass: "++show ass++"\nin calculation of c = a `lub` b with a="++show a++" and b="++show b) else
                a `lub` b
            b = (target.last.last) ass
   -- It is imperative that both ass and css are not empty.
     move :: [Expressions] -> [Expressions] -> [([Expressions],[Expressions])]
     move ass [] = [(ass,[])]
     move ass css
      = (ass,css):
        if and ([not (idsOnly (F cs))| cs<-css]) -- idsOnly (F [])=True, so:  and [not (null cs)| cs<-css]
        then [ts| length (eqClass (==) (map head css)) == 1
                , isUni h
                , ts<-move [[flp h]++as|as<-ass] (map tail css)]++
             [ts| length (eqClass (==) (map last css)) == 1
                , isInj l
                , ts<-move [as++[flp l]|as<-ass] (map init css)]
        else []
        where h=head (map head css); l=head (map last css)

   shiftR :: Expression -> Expressions
   shiftR r
    | length antss+length conss /= length fus = error ("!Fatal (module Calc 106): shiftR will not handle argument of the form "++showADL r)
    | null antss || null conss                = [disjuncts r|not (null fs)] --  shiftR doesn't work here.
    | idsOnly conss                           = [Fux ([Cpx (F [Tm (mIs srcA)(-1)])]++map F antss)]
    | otherwise                               = [Fux ([ Cpx (F (if null ts then id' css else ts))
                                                     | ts<-ass++if null ass then [id' css] else []]++
                                                     [ F (if null ts then id' ass else ts)
                                                     | ts<-css++if null css then [id' ass] else []])
                                                | (ass,css)<-rd(move antss conss)]
    where
     Fux fs = disjuncts r
     fus = filter (not.isIdent) fs
     antss = [ts | Cpx (F ts)<-fus]
     conss = [ts | F ts<-fus]
     srcA = if null conss then error ("!Fatal (module Calc 119): empty consequent in shiftR ("++showADL r++")") else
            if length (eqClass order [ source (head cons) | cons<-conss])>1
            then error ("Fatal (module Calc120): shiftR ("++showADL r++")\nin calculation of srcA\n"++show (eqClass order [ source (head cons) | cons<-conss]))
            else foldr1 lub [ source (head cons) | cons<-conss]
     id' css = [Tm (mIs c) (-1)]
      where a = (source.head.head) css
            c = if not (a `order` b)
                then error ("!Fatal (module Calc 126): shiftR ("++showADL r++")\nass: "++show css++"\nin calculation of c = a `lub` b with a="++show a++" and b="++show b ++ ". " )
                else a `lub` b
            b = (target.last.last) css
     move :: [Expressions] -> [Expressions] -> [([Expressions],[Expressions])]
     move [] css = [([],css)]
     move ass css
      = (ass,css):
        if and [not (null as)| as<-ass]
        then [ts| length (eqClass (==) (map head ass)) == 1
                , isSur h
                , ts<-move (map tail ass) [[flp h]++cs|cs<-css]]++
             [ts| length (eqClass (==) (map last ass)) == 1
                , isTot l
                , ts<-move (map init ass) [cs++[flp l]|cs<-css]]
        else []
        where h=head (map head ass); l=head (map last ass)

-- Deze functie neemt verschillende clauses samen met het oog op het genereren van code.
-- Hierdoor kunnen grotere brokken procesalgebra worden gegenereerd.
   assembleECAs :: (Morphism->Bool) -> [Quad] -> [ECArule]
   assembleECAs visible qs
    = [ecarule i| (ecarule,i) <- zip ecas [(1::Int)..]]
      where
       mphEqCls = eqCl fst4 [(m,shifts,conj,cl_rule ccrs)| Quad m ccrs<-qs, (conj,shifts)<-cl_conjNF ccrs]
       ecas
        = [ ECA (On ev m) delt act
          | mphEq <- mphEqCls
          , let (m,_,_,_) = head mphEq
          , let Tm delt _ = delta (sign m)
          , ev<-[Ins,Del]
          , let act = All [ Chc [ (if isTrue  clause'   then Nop else
                                   if isTrue  step      then Nop else
                                   if isFalse clause'   then Blk else
--                                 if not (visible m) then Blk else
                                   doCode visible ev toExpr viols)
                                   [(conj,causes)]  -- the motivation for these actions
                                | clause@(Fux fus) <- shifts
                                , let clause' = conjNF (subst (m, actSem Ins m (delta (sign m))) clause)
                                , let step    = conjNF (Fux[Cpx clause,clause'])
                                , let viols   = conjNF (notCp clause')
                                , let negs    = Fux [f| f<-fus, isNeg f]
                                , let poss    = Fux [f| f<-fus, isPos f]
                                , let frExpr  = if ev==Ins
                                                then conjNF negs
                                                else conjNF poss
                                , m `elem` map makeInline (mors frExpr)
                                , let toExpr = if ev==Ins
                                               then conjNF poss
                                               else conjNF (notCp negs)
                                ]
                                [(conj,causes)]  -- to supply motivations on runtime
                          | conjEq <- eqCl snd3 [(shifts,conj,rule)| (_,shifts,conj,rule)<-mphEq]
                          , let causes          = rd' nr (map thd3 conjEq)
                          , let (shifts,conj,_) = head conjEq
                          ]
                          [(conj,rd' nr [r|(_,_,_,r)<-cl])| cl<-eqCl thd4 mphEq, let (_,_,conj,_) = head cl]  -- to supply motivations on runtime
          ]
       fst4 (w,_,_,_) = w
       snd3 (_,y,_) = y
       thd3 (_,_,z) = z
       thd4 (_,_,z,_) = z

-- If one rule r blocks upon an event, e.g. e@(ON Ins m), while another ECA rule r'
-- maintains something else with that same event e, we can save r' the trouble.
-- After all, event e will block anyway.
-- preEmpt tries to simplify ECArules by predicting whether a rule will block.
   preEmpt :: [ECArule] -> [ECArule]
   preEmpt ers = pr [length ers] (10::Int)
    where
     pr :: [Int] -> Int -> [ECArule]
     pr ls n
       | n == 0     = error ("!Fatal (module ADL2Fspec 674): too many cascading levels in preEmpt "++show ls)
       | (not.null) cascaded = pr (length cascaded:ls)
                               -- ([er{ecaAction=normPA (ecaAction er)}| er<-cascaded] ++uncasced)
                                  (n-1)
       | otherwise           = [er{ecaAction=normPA (ecaAction er)}| er<-uncasced]
      where
-- preEmpt divides all ECA rules in uncascaded rules and cascaded rules.
-- cascaded rules are those rules that have a Do component with event e, where e is known to block (for some other reason)
       new  = [er{ecaAction=normPA (ecaAction er)}| er<-ers]
       cascaded = [er{ecaAction=action'}| er<-new, let (c,action') = cascade (eMhp (ecaTriggr er)) (ecaAction er), c]
       uncasced = [er|                   er<-new, let (c,_)      = cascade (eMhp (ecaTriggr er)) (ecaAction er), not c]
-- cascade inserts a block on the place where a Do component exists that matches the blocking event.
     cascade :: Morphism -> PAclause -> (Bool, PAclause)
     cascade mph (Do srt (Tm to _) _ _) | (not.null) blkErs = (True, ecaAction (head blkErs))
      where blkErs = [er| er<-ers
                        , Blk _<-[ecaAction er]
                        , let t = ecaTriggr er
                        , eSrt t==srt
                        , makeInline (eMhp t) == makeInline to
                        , makeInline mph      /= makeInline to
                        ]
     cascade  _  c@Do{}           = (False, c)
     cascade mph (New c clause m) = ((fst.cascade mph.clause) "dummystr", New c (\str->(snd.cascade mph.clause) str) m)
     cascade mph (Rmv c clause m) = ((fst.cascade mph.clause) "dummystr", Rmv c (\str->(snd.cascade mph.clause) str) m)
     cascade mph (Sel c e cl m)   = ((fst.cascade mph.cl) "dummystr",     Sel c e (\str->(snd.cascade mph.cl) str)   m)
     cascade mph (Chc ds m)       = (or (map (fst.cascade mph) ds), Chc (map (snd.cascade mph) ds) m)
     cascade mph (All ds m)       = (or (map (fst.cascade mph) ds), All (map (snd.cascade mph) ds) m)
     cascade  _  (Nop m)          = (False, Nop m)
     cascade  _  (Blk m)          = (False, Blk m)

   conjuncts :: Rule -> Expressions
   conjuncts = fiRule.conjNF.normExpr
    where fiRule (Fix fis) = {- map disjuncts -} fis
          fiRule r        = [ {- disjuncts -} r]

-- The function disjuncts yields an expression which has constructor Fu in every case.
   disjuncts :: Expression -> Expression
   disjuncts = fuRule
    where fuRule (Fux cps) = (Fux . rd . map cpRule) cps
          fuRule r        = Fux [cpRule r]
          cpRule (Cpx r)   = Cpx (fRule r)
          cpRule r        = fRule r
          fRule (F ts)    = F ts
          fRule  r        = F [r]

   actSem :: InsDel -> Morphism -> Expression -> Expression
   actSem Ins m (Tm d _) | makeInline m==makeInline d = Tm m (-1)
                       | otherwise                  = Fux[Tm m (-1),Tm d (-1)]
   actSem Ins m delt   = disjNF (Fux[Tm m (-1),delt])
   actSem Del m (Tm d _) | makeInline m==makeInline d = Fix[]
                       | otherwise                  = Fix[Tm m (-1), Cpx (Tm d (-1))]
   actSem Del m delt   = conjNF (Fix[Tm m (-1),Cpx delt])
 --  actSem Del m delt = Fi[m,Cp delt]

   delta :: (Concept, Concept) -> Expression
   delta (a,b)  = Tm (makeMph (Sgn { decnm   = "Delta"
                                   , desrc   = a
                                   , detrg   = b
                                   , decprps = []
                                   , decprps_calc = []
                                   , decprL  = ""
                                   , decprM  = ""
                                   , decprR  = ""
                                   , decpopu = []
                                   , decexplain = ""  -- obsolete as of 18 July 2010
                                   , decfpos = Nowhere
                                   , decid   = 0
                                   , deciss  = True
                                   , decusr  = False
                                   , decpat  = ""
                                   })) (-1)

   -- | de functie doCode beschrijft de voornaamste mogelijkheden om een expressie delta' te verwerken in expr (met tOp'==Ins of tOp==Del)
-- TODO: Vind een wetenschappelijk artikel waar de hier beschreven transformatie uitputtend wordt behandeld.
-- TODO: Deze code is onvolledig en misschien zelfs fout....
   doCode :: (Morphism->Bool)        --  the morphisms that may be changed
          -> InsDel
          -> Expression              --  the expression in which a delete or insert takes place
          -> Expression              --  the delta to be inserted or deleted
          -> [(Expression,Rules )]   --  the motivation, consisting of the conjuncts (traced back to their rules) that are being restored by this code fragment.
          -> PAclause
   doCode editAble tOp' expr1 delta1 motive = doCod delta1 tOp' expr1 motive
    where
      doCod deltaX tOp exprX motiv =
        case (tOp, exprX) of
          (_ ,  Fux [])   -> Blk motiv
          (_ ,  Fix [])   -> Nop motiv
          (_ ,  F [])    -> error ("!Fatal (module Calc 366): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (F [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++").")
          (_ ,  Fdx [])   -> error ("!Fatal (module Calc 368): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (Fdx [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++").")
          (_ ,  Fux [t])  -> doCod deltaX tOp t motiv
          (_ ,  Fix [t])  -> doCod deltaX tOp t motiv
          (_ ,  F [t])   -> doCod deltaX tOp t motiv
          (_ ,  Fdx [t])  -> doCod deltaX tOp t motiv
          (Ins, Cpx x)    -> doCod deltaX Del x motiv
          (Del, Cpx x)    -> doCod deltaX Ins x motiv
          (Ins, Fux fs)   -> Chc [ doCod deltaX Ins f motiv | f<-fs{-, not (f==expr1 && Ins/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Ins, Fix fs)   -> All [ doCod deltaX Ins f []    | f<-fs ] motiv
          (Ins, F ts)    -> Chc [ if F ls==flp (F rs)
                                  then Chc [ New c fLft motiv
                                           , Sel c (F ls) fLft motiv
                                           ] motiv
                                  else Chc [ New c (\x->All [fLft x, fRht x] motiv) motiv
                                           , Sel c (F ls) fLft motiv
                                           , Sel c (flp(F rs)) fRht motiv
                                           ] motiv
                                | (ls,rs)<-chop ts
                                , let c = source (F rs) `lub` target (F ls)
                                , let fLft = (\atom->doCod (disjNF (Fux[F [Tm (Mp1 atom [] c)(-1),v (c,source deltaX),deltaX],Cpx (F rs)])) Ins (F rs) [])
                                , let fRht = (\atom->doCod (disjNF (Fux[F [deltaX,v (target deltaX,c),Tm (Mp1 atom [] c)(-1)],Cpx (F ls)])) Ins (F ls) [])
                                ] motiv
          (Del, F ts)    -> Chc [ if F ls==flp (F rs)
                                  then Chc [ Sel c (disjNF (F ls)) (\_->Rmv c fLft motiv) motiv
                                           , Sel c (disjNF (F ls)) fLft motiv
                                           ] motiv
                                  else Chc [ Sel c (disjNF (Fix [F ls,flp(F rs)])) (\_->Rmv c (\x->All [fLft x, fRht x] motiv) motiv) motiv
                                           , Sel c (disjNF (Fix [F ls,flp(F rs)])) fLft motiv
                                           , Sel c (disjNF (Fix [F ls,flp(F rs)])) fRht motiv
                                           ] motiv
                                | (ls,rs)<-chop ts
                                , let c = source (F rs) `lub` target (F ls)
                                , let fLft = (\atom->doCod (disjNF (Fux[F [Tm (Mp1 atom [] c)(-1),v (c,source deltaX),deltaX],Cpx (F rs)])) Del (F rs) [])
                                , let fRht = (\atom->doCod (disjNF (Fux[F [deltaX,v (target deltaX,c),Tm (Mp1 atom [] c)(-1)],Cpx (F ls)])) Del (F ls) [])
                                ] motiv
          (Del, Fux fs)   -> All [ doCod deltaX Del f []    | f<-fs{-, not (f==expr1 && Del/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Del, Fix fs)   -> Chc [ doCod deltaX Del f motiv | f<-fs ] motiv
-- Op basis van de Morgan is de procesalgebra in het geval van (Ins, Fd ts)  afleidbaar uit uit het geval van (Del, F ts) ...
          (_  , Fdx ts)   -> doCod deltaX tOp (Cpx (F (map Cpx ts))) motiv
          (_  , K0x x)    -> doCod (deltaK0 deltaX tOp x) tOp x motiv
          (_  , K1x x)    -> doCod (deltaK1 deltaX tOp x) tOp x motiv
          (_  , Tm m _)  -> -- error ("DIAG ADL2Fspec 824:\ndoCod ("++showADL deltaX++") "++show tOp++" ("++showADL exprX++"),\n"
                                   -- -- ++"\nwith disjNF deltaX:\n "++showADL (disjNF deltaX))
                            (if editAble m then Do tOp exprX (deltaX) motiv else Blk [(Tm m (-1),rd' nr [r|(_,rs)<-motiv, r<-rs])])
          (_ , _)        -> error ("!Fatal (module Calc 827): Non-exhaustive patterns in the recursive call doCod ("++showADL deltaX++") "++show tOp++" ("++showADL exprX++"),\n"++
                                   "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++").")

   chop :: [t] -> [([t], [t])]
   chop [_]    = []
   chop (x:xs) = ([x],xs): [(x:l, r)| (l,r)<-chop xs]
   chop []     = []

   deltaK0 :: t -> InsDel -> t1 -> t
   deltaK0 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta |- x*)
   deltaK0 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x* leeg is)
   deltaK1 :: t -> InsDel -> t1 -> t
   deltaK1 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta |- x+)
   deltaK1 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x+ leeg is)

