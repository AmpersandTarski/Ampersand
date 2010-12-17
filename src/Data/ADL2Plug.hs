module Data.ADL2Plug (mor2plug,makePlugs,makePhpPlug,makeSqlPlug)
where
import Collection     (Collection((>-)))
import Adl
import Auxiliaries    (eqCl, sort')
import Data.Plug
import Prototype.CodeAuxiliaries (Named(..))
import Prototype.CodeVariables (CodeVar(..),CodeVarIndexed(..))
import Char
import FPA
import Data.Maybe (listToMaybe)

-- mor2plug creates associations between plugs that represent wide tables.
-- this concerns relations that are not univalent nor injective,
-- Univalent relations and injective relations cannot be associations, because they are used as attributes in wide tables.
mor2plug :: Morphism -> [Morphism] -> PlugSQL
mor2plug  m totals
         = if Inj `elem` mults || Uni `elem` mults then error ("!Fatal (module ADL2Fspec 64): unexpected call of mor2plug("++show m++"), because it is injective or univalent.") else
           if not is_Tot && is_Sur then mor2plug (flp m) totals else
           BinSQL (name m)                                                    -- plname
                  (srcFld,trgFld)                                             -- columns
                  ([(source m,srcFld)| is_Tot]++[(target m,trgFld)| is_Sur])  -- cLkpTbl
                  m                                                           -- mLkp
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
makePlugs :: Context -> Declarations -> [PlugSQL] -> [PlugSQL]
makePlugs context allDecs currentPlugs
 = sort' ((0-).length.fields)
    [ TblSQL (name c)               -- plname
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
           (not (isTot m))                              -- fldnull : can there be empty field-values? (intended for data dictionary of DB-implementation)
                                                        --           Error: only if source m is the I-field of this plug.
           (isInj m)                                    -- flduniq : are all field-values unique? (intended for data dictionary of DB-implementation)
       where fldName = head [nm| (m',nm)<-table, m==m']
             isSQLId = isIdent m && isAuto
             isAuto  = isIdent m
                        && not (null [key| key<-keyDefs context, kdcpt key==target m]) -- if there are any keys around, make this plug autoincrement.
                        && (contents m==Nothing || contents m==Just []) -- and the the field may not contain any strings
             table   = [ entry
                       | cl<-eqCl (map toLower.name) ms
                       , entry<-if length cl==1 then [(r,name r)|r<-cl] else tbl cl]
             tbl rs  = [ entry
                       | cl<-eqCl (map toLower.name.source) rs
                       , entry<-if length cl==1 then [(r,name r++name (source r))|r<-cl] else [(r,name r++show i)|(r,i)<-zip cl [(0::Int)..]]]

-- | makePhpPlug is used to make user defined plugs, with PHP functions to get the data from. Note that these plug's cannot be used to store anything.
makePhpPlug :: ObjectDef -> PlugPHP
makePhpPlug obj
 = PlugPHP (name obj)    -- plname (function name)
           inFile        -- the file in which the plug is located (Nothing means BuiltIn)
           inAttrs       -- the input of this plug (list of arguments)
           outObj        -- the output of this plug (single object or scalar). If inAttrs does not exist, plug should return false.
           verifiesInput -- whether the input of this plug is verified
           fpa           -- the number of function points to be counted for this plug
  where
   inFile :: Maybe String
   inFile = listToMaybe [ file --objstrs = [["FILE=date.plug.php"]]
                        | x<-objstrs obj
                        , str<-x
                        , (m,file)<-[splitAt 5 str]
                        , m=="FILE="
                        ]
   inAttrs :: [CodeVar]
   inAttrs = [toAttr attr | attr<-objats obj, (or$ map (elem "PHPARG") (objstrs attr))]
   toAttr :: ObjectDef -> CodeVar
   toAttr a = CodeVar{cvIndexed=IndexByName -- TODO, read this from parameters
                     ,cvContent=Right [] -- TODO!! Allow complex objects..
                     ,cvExpression=objctx a}
   outObj :: CodeVar
   outObj = CodeVar{cvIndexed=IndexByName
                   ,cvContent=Right [Named (name attr)$ toAttr attr | attr<-objats obj, notElem ["PHPARGS"] (objstrs attr)]
                   ,cvExpression=objctx obj}
   verifiesInput::Bool
   verifiesInput = True
   fpa::FPA
   fpa = (ILGV Eenvoudig)
   

-- | makeSqlPlug is used to make user defined plugs. One advantage is that the field names can be controlled by the user. 
makeSqlPlug :: ObjectDef -> PlugSQL
makeSqlPlug obj
 --TODO151210 -> (see also Instance Object PlugSQL) When is an ObjectDef a ScalarPlug or BinPlug?
 | null(objats obj) && isIdent(objctx obj)
   = ScalarSQL (name obj)  cptfld c NO
 | null(objats obj) --TODO151210 -> assuming objctx obj is Mph{} if it is not I{}
   = error "TODO151210 -> implement defining binary plugs"
 | otherwise
   = TblSQL (name obj)     -- plname (table name)
     makeFields             -- fields
     [(c,cptfld)]           -- cLkpTbl is een lijst concepten die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
     mphflds                -- mLkpTbl is een lijst met morphismen die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
     (ILGV Eenvoudig)       -- functie punten analyse
  where
   --TODO: cptflds and mphflds assume that the user defined plug is a concept plug: 
   --      -> containing just one expression equivalent to the identity relation
   --      -> if the expression is not the identity relation then it is a simple expression (a morphism)
   --      if there are more expressions equivalent to the identity relation then a fatal
   --      the others would probably be ignored by updates and inserts, but fldnull=false => save errors in SQL?
   (c,cptfld) = (\xs->if length xs==1 then head xs else error "!Fatal (module ADL2Fspec 319): Implementation expects only one identity relation in plug.")
             [(source (fldexpr f),f)|f<-makeFields, isIdent(fldexpr f)]
   mphflds = [(m,cptfld,f)|f<-makeFields, length (mors(fldexpr f))==1,m@(Mph{})<-mors(fldexpr f)]
   makeFields ::  [SqlField]
   makeFields =  -- WAAROM?? @Stef: Waarom is hier niet de constructor 'field' (uit Data.Plug) gebruikt??? Volgens mij maakt dat verschil bij de fldauto, maar ik doorgrond het niet helemaal.
-- DAAROM!! @Han: Ik weet het antwoord niet. Ik vermoed dat dit gewoon met 'field' moet.
-- Het veld 'fldauto' geeft aan of het een autoincrement veld moet zijn, en het huidige antwoord daarop (att `elem` autoFields) lijkt me fout.
     [Fld (name att)                 -- fldname : 
          (objctx att)               -- fldexpr : De target van de expressie geeft de waarden weer in de SQL-tabel-kolom.
          (sqltp att)                -- fldtype :
          (not (isTot (objctx att))) -- fldnull : can there be empty field-values? 
          (isInj (objctx att))       -- flduniq : are all field-values unique?
     | att<-objats obj
     ]
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
