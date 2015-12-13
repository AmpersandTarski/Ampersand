module Database.Design.Ampersand.FSpec.FSpecAux 
  (getDeclarationTableInfo,getConceptTableInfo)
where
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Classes.Relational(isTrue)
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms(disjNF)
import Database.Design.Ampersand.FSpec.Plug(plugpath)
import Database.Design.Ampersand.FSpec.FSpec
import Data.List

--WHY bestaat sqlRelPlugs?
-- | sqlRelPlugs levert alle mogelijkheden om een plug met twee velden te vinden waarin (primitieve) expressie e is opgeslagen.
-- | sqlRelPlugs mag alleen gebruikt worden voor primitieve expressies EDcD, EDcI, en EDcV
-- | Als (plug,sf,tf) `elem` sqlRelPlugs fSpec e, dan geldt e = (attExpr sf)~;(attExpr tf)
-- | Als sqlRelPlugs fSpec e = [], dan volstaat een enkele tabel lookup niet om e te bepalen
-- | Opletten dus, met de nieuwe ISA-structuur van 2013, omdat daarin tabellen bestaan met disjuncte verzamelingen...
sqlRelPlugs :: FSpec -> Expression  -> [(PlugSQL,SqlAttribute,SqlAttribute)] --(plug,source,target)
sqlRelPlugs fSpec e
   = [ (plug,fld0,fld1)
     | InternalPlug plug<-plugInfos fSpec
     , (fld0,fld1)<-sqlPlugAttributes fSpec plug e
     ]

-- return table name and source and target column names for relation rel, or nothing if the relation is not found
getDeclarationTableInfo :: FSpec -> Declaration -> (PlugSQL,SqlAttribute,SqlAttribute)
getDeclarationTableInfo fSpec decl =
 case decl of
   Sgn{}   -> case sqlRelPlugs fSpec (EDcD decl) of
                    [plugInfo] -> plugInfo
                    []         -> fatal 34 $ "Reference to a non-existing plug: "++show (EDcD decl)
                    [(t1,src1,trg1),(t2,src2,trg2)]
                       -> if t1 ==t2 && src1 == trg2 && trg1 == src2
                          then (t1,src1,trg1)
                          else fatal 426 $ "Multiple plugs for relation "++ show decl ++"\n" ++
                                    intercalate "\n\n" (map showPInfo [(t1,src1,trg1),(t2,src2,trg2)])
                    pinfos     -> fatal 428 $ "Multiple plugs for relation "++ show decl ++"\n" ++
                                    intercalate "\n\n" (map showPInfo pinfos)
   Isn cpt -> case sqlRelPlugs fSpec (EDcI cpt) of
                    plugInfo:_ -> plugInfo        -- There may be multiple plugInfo's for concepts. This is not a problem.
                    []         -> fatal 44 $ "Reference to a non-existing plug: "++show (EDcI cpt)
   _       -> fatal 420 "getDeclarationTableInfo must not be used on this type of declaration!"
   where
    showPInfo (tab, src, trg) = intercalate "  \n"
                                 [ "Table: "++name tab
                                 , "  sourceAttribute: "++attName src
                                 , "  targetAttribute: "++attName trg
                                 ]


getConceptTableInfo :: FSpec -> A_Concept -> (PlugSQL,SqlAttribute)
getConceptTableInfo fSpec cpt 
  = case lookupCpt fSpec cpt of
      []    -> fatal 55 $ "No plug found for concept '"++name cpt++"'."
      (x:_) -> x  --Any of the resulting plugs should do. 

--iff proven that e is equivalent to plugexpr
--   AND not proven that e is not equivalent to plugexpr
--then return (fld0,fld1)
--TODO -> can you prove for all e whether e is equivalent to plugexpr or not?
sqlPlugAttributes :: FSpec -> PlugSQL -> Expression  -> [(SqlAttribute, SqlAttribute)]
sqlPlugAttributes fSpec p e' =
    let e = disjNF (getOpts fSpec) e' -- SJ20140207 Why is this normalization necessary?
    in nub
        [(fld0,fld1)
        | fld0<-[f |f<-plugAttributes p,target (attExpr f)==source e] --fld0 must be a attribute matching the source of e
        , fld1<-[f |f<-plugAttributes p,target (attExpr f)==target e] --fld1 must be a attribute matching the target of e
        , Just plugexpr <- [plugpath p fld0 fld1] --the smallest expression from fld0 to fld1 (both in same plug)
        , let se = attExpr fld0
              te = attExpr fld1
              bs = (isTrue.disjNF (getOpts fSpec)) (notCpl e .\/. flp se .:. te)    --       e |- se~;te
              bt = (isTrue.disjNF (getOpts fSpec)) (notCpl (flp se .:. te) .\/. e)  --       se~;te |- e
        , --reasons why e is equivalent to plugexpr:
           --because e and plugexpr are equal
           e==plugexpr
     --   || because1 e fld0 fld1
     --OR e is equivalent to plugexpr for some other reason (requires reasoning)
        || bs && bt     ]                                          --       e = se~;te
        {- the above should be enough.. but the relation algebra calculations
           are not good enough yet. In particular:
             isFalse ((I/\x);e /\ -e)
           and
             isTrue  ((I/\e;e~);e \/ -e)
           do not work (these should yield True instead of False in both cases)

           The code below fixes exactly these ommissions
      --
        || (isProp (se) && (te == e)
           && (isTrue$disjNF (getOpts fSpec)$ let c = source e in (EDcI c ./\. simplF [e,flp e] ) .\/. notCpl se))
        || (isProp (te) && se==flp e
           && (isTrue$disjNF (getOpts fSpec)$ let c = source e in (EDcI c ./\. simplF [e,flp e] ) .\/. notCpl te))
        -- found another exception:
        --     isFalse (I;I /\ -I)
        --   and
        --     isTrue  (I;I \/ -I)
        --   yield False, but should yield True
        --
        || (  (se == te) && isIdent e && (isSur se)  )
        , --TODO -> reasons why e is not equivalent to plugexpr:
        True
        ]
  where
  -- simplF: replace a;a~ by I if INJ&TOT
  simplF ks = simplify ( if null fs || null (head fs) then replF ks else replF $ head fs )
    where fs = [ts | ECps ts <- [simplify $ ECps ks]] -- if null, replF will probably not do a lot.
  simplF ks = case simplify (foldr1 .:. ks) of
                 t@ECps{} -> simplify (replF (exprCps2list t))
                 _        -> simplify (replF ks)
           -- null occurs especialy in cases of [I;e] and [e;I]

  replF [k:k2]    | k == flp k2 && isInj k && isTot k = EDcI (source k)
  replF (k:k2:ks) | k == flp k2 && isInj k && isTot k = replF ks
  replF [a]                                           = a
  replF (k:k2:ks) | fs /= [k2:ks]
   = case res of ECps{} -> replF (exprCps2list res) ; _ -> ECps (k,res)
     where res = replF (k2:ks)
           fs  = case res of ECps{} -> [exprCps2list res] ; _ -> []

  replF [] -- this should not occur here, and if it does, it might cause errors in other code that should be solved here
   = fatal 542 "Could not define a properly typed I for ECps[] in replF in sqlPlugAttributes in Prototype/RelBinGenSQL.hs"
           -- this error does not guarantee, however, that simplF yields no ECps []. In particular: simplify (ECps [I;I]) == ECps []
  replF ks = ECps (ks)
  -----------------
  -}
                                 