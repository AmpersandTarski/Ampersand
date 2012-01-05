{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
module DatabaseDesign.Ampersand.Input.ADL1.CCv221 
   (pContext, pIncludeFile, pPopulations,pExpr, keywordstxt, keywordsops, specialchars, opchars) where
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner
            ( Token(..),TokenType(..),noPos
            , pKey,pConid,pString,pSpec,pAtom,pExpl,pVarid,pComma,pInteger)
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing
            (Parser
            , (<$>) , (<$), (<*>), (<*), (*>), (<|>), (<??>)
            ,pList,pListSep,pList1,pList1Sep,pSym
            ,pSucceed
            ,opt, Sequence,Alternative, IsParser
            )
   import DatabaseDesign.Ampersand.Basics  (fatalMsg,Collection(..),trim)
   import DatabaseDesign.Ampersand.Core.ParseTree    
   import Data.Char (toUpper)
--   import DatabaseDesign.Ampersand.Misc         (Lang(..),defaultFlags, Options(..))
--   import DatabaseDesign.Ampersand.Misc.Explain
   import Data.List (nub,sort)
   fatal :: Int -> String -> a
   fatal = fatalMsg "ADL1.CCv221"

--  The Ampersand scanner takes the file name (String) for documentation and error messaging.
--   scanner :: String -> String -> [Token]
--   scanner fn str = scan keywordstxt keywordsops specialchars opchars fn initPos str

   keywordstxt :: [String]
   keywordstxt       = [ "INCLUDE"
                       , "CONTEXT", "ENDCONTEXT", "EXTENDS", "TEXTMARKUP", "THEMES"
                       , "PATTERN", "ENDPATTERN"
                       , "PROCESS", "ENDPROCESS"
                       , "INTERFACE", "FOR", "BOX", "INITIAL", "SQLPLUG", "PHPPLUG", "TYPE"
                       , "POPULATION", "CONTAINS"
                       , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX", "IRF", "PROP", "ALWAYS"
                       , "RULE", "TEST"
                       , "RELATION", "MEANING", "MESSAGE", "CONCEPT", "KEY"
                       , "IMPORT", "SPEC", "ISA", "I", "V"
                       , "PRAGMA", "EXPLAIN", "PURPOSE", "IN", "REF", "ENGLISH", "DUTCH"
                       , "ONE", "BIND", "TOPHP", "BINDING"
                       , "BYPLUG"
                       , "ROLE", "EDITS", "MAINTAINS"
                       ]
   keywordsops :: [String]
   keywordsops       = [ "-|", "|-", "-", "->", ">", "=", "~", "+", ";", "!", "*", "::", ":", "\\/", "/\\", "\\", "/", "<>" ]
   specialchars :: String
   specialchars      = "()[].,{}"
   opchars :: String
   opchars           = nub (sort (concat keywordsops))

   --to parse files containing only populations
   pPopulations :: Parser Token [P_Population]
   pPopulations = pList1 pPopulation

   pBind             :: Parser Token (P_Declaration,String)
   pBind              = rebuild <$ pKey "BIND" <*> pDeclaration <* pKey "TOPHP" <*> (pConid <|> pString)
                       where rebuild d s = (d,s)

   pBindings :: Parser Token [(P_Declaration,String)]
   pBindings = (pKey "BINDING" *> pList1Sep (pSpec ',') pBind) `opt` []
   
   pContext         :: Parser Token (P_Context, [String]) -- the result is the parsed context and a list of include filenames
   pContext  = rebuild <$ pKey "CONTEXT" <*> pConid
                            <*> pList pIncludeStatement 
                            <*> optional pLanguageID 
                            <*> optional pFormatID 
                            <*> optional (rebexpr <$ pKey ":" <*> pExpr <*> pBindings ) <*>
                              --    ((pKey "EXTENDS" *> pList1Sep (pSpec ',') pConid) `opt` []) <*>
                               pList pContextElement <* pKey "ENDCONTEXT"
                       where
                       rebexpr :: P_Expression -> [(P_Declaration, String)] -> (P_Expression , [(P_Declaration,String)])
                       rebexpr x y = (x,y)
                       rebuild :: String -> [String] -> Maybe Lang -> Maybe PandocFormat -> Maybe (P_Expression, [(P_Declaration, String)]) -> [ContextElement] -> (P_Context, [String])
                       rebuild nm includeFileNames lang fmt env ces = 
                         (PCtx{ ctx_nm    = nm
                              , ctx_lang  = lang
                              , ctx_markup= fmt
                              , ctx_thms  = (nub.concat) [xs | CThm xs<-ces] -- Names of patterns/processes to be printed in the functional specification. (For partial documents.)
                              , ctx_pats  = [p | CPat p<-ces]       -- The patterns defined in this context
                              , ctx_PPrcs = [p | CPrc p<-ces]       -- The processes as defined by the parser
                              , ctx_rs    = [p | CRul p<-ces]       -- All user defined rules in this context, but outside patterns and outside processes
                              , ctx_ds    = [p | CDcl p<-ces]       -- The declarations defined in this context, outside the scope of patterns
                              , ctx_cs    = [c | CCon c<-ces]       -- The concept definitions defined in this context, outside the scope of patterns
                              , ctx_ks    = [k | CKey k<-ces]       -- The key definitions defined in this context, outside the scope of patterns
                              , ctx_gs    = [g | CGen g<-ces]       -- The gen definitions defined in this context, outside the scope of patterns
                              , ctx_ifcs  = [s | Cifc s<-ces]       -- The interfaces defined in this context, outside the scope of patterns
                              , ctx_ps    = [e | CPrp e<-ces]       -- The pre-explanations defined in this context, outside the scope of patterns
                              , ctx_pops  = [p | CPop p<-ces]       -- The populations defined in this contextplug<-ces]  
                              , ctx_sql   = [p | CSqlPlug p<-ces]   -- user defined sqlplugs, taken from the Ampersand scriptplug<-ces]  
                              , ctx_php   = [p | CPhpPlug p<-ces]   -- user defined phpplugs, taken from the Ampersand script
                              , ctx_env   = env                     -- an expression on the context with unbound relations, to be bound in this environment
                              }
                          , includeFileNames)

   data ContextElement = CPat P_Pattern
                       | CPrc P_Process
                       | CRul P_Rule
                       | CDcl P_Declaration
                       | CCon ConceptDef
                       | CKey P_KeyDef
                       | CGen P_Gen
                       | Cifc P_Interface
                       | CPop P_Population
                       | CSqlPlug P_ObjectDef
                       | CPhpPlug P_ObjectDef
                       | CPrp PPurpose
                       | CThm [String]           -- a list of themes to be printed in the functional specification. These themes must be PATTERN or PROCESS names.

   pIncludeStatement :: Parser Token String
   pIncludeStatement = pKey "INCLUDE" *> pString

   -- this parser returns a function which, when applied to a P_Context, adds all context elements from the parsed include file
   -- in front of the context elements in that P_Context.
   --
   -- The included file is surrounded with CONTEXT <name> and ENDCONTEXT to allow included files to be compiled standalone.
   -- However, the body may consist only of context elements, so no include statements, language ids, etc. 
   pIncludeFile         :: Parser Token (P_Context -> P_Context)
   pIncludeFile  = addElementsToContext <$ pKey "CONTEXT" <* pConid <*> pList pContextElement <* pKey "ENDCONTEXT"
    where addElementsToContext includedCtxtElts = \originalContext ->
            originalContext { ctx_thms  = nub $ concat [xs | CThm xs<-includedCtxtElts] ++ ctx_thms originalContext  
                            , ctx_pats  = [p | CPat p<-includedCtxtElts] ++ ctx_pats originalContext
                            , ctx_PPrcs = [p | CPrc p<-includedCtxtElts] ++ ctx_PPrcs originalContext
                            , ctx_rs    = [p | CRul p<-includedCtxtElts] ++ ctx_rs originalContext
                            , ctx_ds    = [p | CDcl p<-includedCtxtElts] ++ ctx_ds originalContext
                            , ctx_cs    = [c | CCon c<-includedCtxtElts] ++ ctx_cs originalContext
                            , ctx_ks    = [k | CKey k<-includedCtxtElts] ++ ctx_ks originalContext
                            , ctx_gs    = [g | CGen g<-includedCtxtElts] ++ ctx_gs originalContext
                            , ctx_ifcs  = [s | Cifc s<-includedCtxtElts] ++ ctx_ifcs originalContext
                            , ctx_ps    = [e | CPrp e<-includedCtxtElts] ++ ctx_ps originalContext
                            , ctx_pops  = [p | CPop p<-includedCtxtElts] ++ ctx_pops originalContext
                            , ctx_sql   = [p | CSqlPlug p<-includedCtxtElts] ++ ctx_sql originalContext
                            , ctx_php   = [p | CPhpPlug p<-includedCtxtElts] ++ ctx_php originalContext
                            } 

   
   pFormatID    :: Parser Token PandocFormat
   pFormatID     = f <$> (pKey "TEXTMARKUP" *> pConid)
                         where
                          f str = case map toUpper str of
                                      "REST"     -> ReST
                                      "HTML"     -> HTML
                                      "LATEX"    -> LaTeX
                                      "MARKDOWN" -> Markdown
                                      _ -> fatal 113 (if null str then "must specify a markup format" else "markup format "++str++" is not supported")

   pPrintThemes       :: Parser Token [String]
   pPrintThemes        = pKey "THEMES" *> pList1Sep (pSpec ',') (pConid <|> pString)

   pLanguageID        :: Parser Token Lang
   pLanguageID         = lang <$> (pKey "IN" *> (pKey "DUTCH" <|> pKey "ENGLISH"))
                         where
                          lang str = case str of
                                      "DUTCH"      -> Dutch
                                      "ENGLISH"    -> English
                                      _ -> fatal 141 (if null str then "must specify a language" else "language "++str++" is not supported")

   pRefID             :: Parser Token String
   pRefID              = (pKey "REF" *> pString) `opt` []

   pPurpose           :: Parser Token PPurpose
   pPurpose            = rebuild <$> pKey_pos "EXPLAIN" <*> pRef2Obj <*> optional pLanguageID <*> optional pFormatID <*> pRefID <*> pExpl      <|>  -- syntax will become obsolete
                         rebuild <$> pKey_pos "PURPOSE" <*> pRef2Obj <*> optional pLanguageID <*> optional pFormatID <*> pRefID <*> pExpl
                         where
                           rebuild orig obj lang fmt ref str
                             = PRef2 orig obj (P_Markup lang fmt str) ref
   pRef2Obj           :: Parser Token PRef2Obj
   pRef2Obj            = PRef2ConceptDef  <$ pKey "CONCEPT"   <*> (pConid <|> pString)          <|>
                         pRef2Declaration <$ pKey "RELATION"  <*> pRelation <*> optional pSign  <|>
                         PRef2Rule        <$ pKey "RULE"      <*> pADLid                        <|>
                         PRef2KeyDef      <$ pKey "KEY"       <*> pADLid                        <|>  
                         PRef2Pattern     <$ pKey "PATTERN"   <*> pADLid                        <|>
                         PRef2Process     <$ pKey "PROCESS"   <*> pADLid                        <|>
                         PRef2Interface   <$ pKey "INTERFACE" <*> pADLid                        <|>
                         PRef2Context     <$ pKey "CONTEXT"   <*> pADLid
                         where pRef2Declaration nm Nothing    = PRef2Declaration nm P_Sign {psign=[] }
                               pRef2Declaration nm (Just psgn)= PRef2Declaration nm psgn

   pContextElement    :: Parser Token ContextElement
   pContextElement     = CPat     <$> pPattern      <|>
                         CPrc     <$> pProcess      <|>
                         CRul     <$> pRule         <|>
                         CDcl     <$> pDeclaration  <|>
                         CCon     <$> pConceptDef   <|>
                         CKey     <$> pKeyDef       <|>
                         Cifc     <$> pInterface    <|>
                         CSqlPlug <$> pSqlplug      <|>
                         CPhpPlug <$> pPhpplug      <|>
                         CPrp     <$> pPurpose      <|>
                         CPop     <$> pPopulation   <|>
                         CThm     <$> pPrintThemes

   pPopulation         :: Parser Token P_Population
   pPopulation = ppop <$ pKey "POPULATION" <*> pRelation <*> optional pSign <* pKey "CONTAINS" <*> pContent
                 where ppop r  Nothing   c = P_Popu r [] c
                       ppop r (Just sgn) c = P_Popu r (psign sgn) c

   pPattern         :: Parser Token P_Pattern
   pPattern  = rebuild <$> pKey_pos "PATTERN" <*> (pConid <|> pString)
                       <*> pList pPatElem
                       <*> pKey_pos "ENDPATTERN"
                       where
                         rebuild :: Origin -> String -> [PatElem] -> Origin -> P_Pattern
                         rebuild pos' nm pes end
                          = P_Pat { pt_nm  = nm
                                  , pt_pos = pos'
                                  , pt_end = end
                                  , pt_rls = [r | Pr r<-pes]
                                  , pt_gns = [g | Pg g<-pes]
                                  , pt_dcs = [d | Pd d<-pes]
                                  , pt_cds = [c | Pc c<-pes]
                                  , pt_kds = [k | Pk k<-pes]
                                  , pt_xps = [e | Pe e<-pes]
                                  , pt_pop = [p | Pp p<-pes]
                                  } 

   data PatElem      = Pr P_Rule
                     | Pg P_Gen
                     | Pd P_Declaration 
                     | Pc ConceptDef
                     | Pk P_KeyDef
                     | Pe PPurpose
                     | Pp P_Population
                              
   pPatElem         :: Parser Token PatElem
   pPatElem          = Pr <$> pRule         <|>
                       Pg <$> pGen          <|>
                       Pd <$> pDeclaration  <|>
                       Pc <$> pConceptDef   <|>
                       Pk <$> pKeyDef       <|>
                       Pe <$> pPurpose      <|>
                       Pp <$> pPopulation

   pProcess         :: Parser Token P_Process
   pProcess  = rebuild <$> pKey_pos "PROCESS" <*> (pConid <|> pString)
                       <*> pList pProcElem
                       <*> pKey_pos "ENDPROCESS"
                       where
                         rebuild :: Origin -> String -> [ProcElem] -> Origin -> P_Process
                         rebuild pos' nm pes end
                          = P_Prc { procNm    = nm
                                  , procPos   = pos'
                                  , procEnd   = end
                                  , procRules = [rr | PrR rr<-pes]
                                  , procGens  = [g  | PrG g <-pes]
                                  , procDcls  = [d  | PrD d <-pes]
                                  , procRRuls = [rr | PrM rr<-pes]
                                  , procRRels = [rr | PrL rr<-pes]
                                  , procCds   = [cd | PrC cd<-pes]
                                  , procKds   = [kd | PrK kd<-pes]
                                  , procXps   = [e  | PrE e <-pes]
                                  , procPop   = [p  | PrP p <-pes]
                                  }

   data ProcElem     = PrR P_Rule
                     | PrG P_Gen
                     | PrD P_Declaration
                     | PrM RoleRule
                     | PrL P_RoleRelation
                     | PrC ConceptDef
                     | PrK P_KeyDef
                     | PrE PPurpose
                     | PrP P_Population

   pProcElem        :: Parser Token ProcElem
   pProcElem         = PrR <$> pRule         <|>
                       PrG <$> pGen          <|>
                       PrD <$> pDeclaration  <|>
                       PrM <$> pRoleRule     <|>
                       PrL <$> pRoleRelation <|>
                       PrC <$> pConceptDef   <|>
                       PrK <$> pKeyDef       <|>
                       PrE <$> pPurpose      <|>
                       PrP <$> pPopulation

   pMeaning         :: Parser Token PMeaning
   pMeaning          = rebuild <$ pKey "MEANING" <*> optional pLanguageID <*> optional pFormatID <*> (pString <|> pExpl)
                        where rebuild lang fmt mkup =
                                 PMeaning (P_Markup lang fmt mkup)

   pGen             :: Parser Token P_Gen
   pGen              = rebuild <$ pKey "SPEC" <*> (pConid <|> pString) <*> pKey_pos "ISA" <*> (pConid <|> pString)
                       where rebuild spc p gen = PGen p (PCpt gen) (PCpt spc)

   pRule            :: Parser Token P_Rule
   pRule             = rnm <$> pKey_pos "RULE" <*> pADLid <* pKey ":" <*> pExpr <*> pList pMeaning <*> pList pMessage <|>
                       rnn <$> pKey_pos "RULE" <*>                        pExpr <*> pList pMeaning <*> pList pMessage
                       where
                        --rnn -> rnm with generated name (rulid po)
                        rnn po = rnm po (rulid po)
                        rulid (FileLoc(FilePos (_,Pos l _,_))) = "rule@line"++show l
                        rulid _ = fatal 226 "rulid is expecting a file location."
                        rnm po lbl rexp mean msg
                          = P_Ru { rr_nm  = lbl
                                 , rr_exp = rexp
                                 , rr_fps = po
                                 , rr_mean = mean
                                 , rr_msg = msg
                                 }
                                 
   pMessage         :: Parser Token P_Markup
   pMessage          = P_Markup <$ pKey "MESSAGE" <*> optional pLanguageID <*> optional pFormatID <*> (pString <|> pExpl)


{-  Basically we would have the following expression syntax:
pExpr ::= pExp1   "="    pExp1                           |
          pExp1   "|-"   pExp1                           |
          pExp1 .
pExp1 ::= pList1Sep "/\\" pExp2                          |
          pList1Sep "\\/" pExp2                          |
          pExp2 .
pExp2 ::= pExp3    "-"    pExp3                          |
          pExp3 .
pExp3 ::= pExp4   "\\"   pExp4                           |
          pExp4   "/"    pExp4                           |
          pExp4 .
pExp4 ::= pList1Sep ";" pExp5                            |
          pList1Sep "!" pExp5                            |
          pList1Sep "*" pExp5                            |
          pExp5 .
pExp5 ::= "-"     pExp6                                  |
          pExp6   pSign                                  |
          pExp6   "~"                                    |
          pExp6   "*"                                    |
          pExp6   "+"                                    |
          pExp6 .
pExp6 ::= pRelation                                      |
          "("   pExpr   ")" .
In practice, we have it a little different.
 - In order to avoid "associative" brackets, we parse the associative operators "\/", "/\", ";", and "!" with pList1Sep. That works.
 - We would like the user to disambiguate between "=" and "|-" by using brackets. 
-}

{- In theory, the expression is parsed by:
   pExpr  :: Parser Token (P_Expression)
   pExpr  =  fEequ <$> pExp1  <*  pKey "="   <*>  pExp1   <|>
             fEimp <$> pExp1  <*  pKey "|-"  <*>  pExp1   <|>
             pExp1
             where fEequ lExp rExp = Pequ (lExp, rExp)
                   fEimp lExp rExp = Pimp (lExp, rExp)
-- However elegant, this solution needs to be left-factored in order to get a performant parser.
-}
   pExpr  :: Parser Token P_Expression
   pExpr  =  pExp1 <??> (f <$>  (pKey "=" <|> pKey "|-") <*> pExp1 )
             where f "="  rExp lExp = Pequ (lExp, rExp)
                   f _    rExp lExp = Pimp (lExp, rExp)
             
{- The union and intersect are parsed as lists. The obvious thing to do might be:
   pExp1  :: Parser Token P_Expression
   pExp1   = fEuni <$> pList1Sep (pKey "\\/") pExp1a
             where fEuni [x] = x
                   fEuni xs  = PUni xs

   pExp1a :: Parser Token P_Expression
   pExp1a  = fEisc <$> pList1Sep (pKey "/\\") pExp2
             where fEisc [x] = x
                   fEisc xs  = Pisc xs
However, we want intersect and union to be of equal precedence. It has to be left-factored
and the grammar must be disambiguated in order to get a performant parser...
-}

   pExp1  :: Parser Token P_Expression
   pExp1   = f <$> pExp2 <*> (pLuni <|> pLisc)
             where f x (PUni []) = x
                   f x (Pisc []) = x
                   f x (PUni xs) = PUni (x:xs)
                   f x (Pisc xs) = Pisc (x:xs)
                   f _ _ = fatal 284 "PUni Pisc expected"
                   pLuni = PUni <$> pList1 (pKey "\\/" *> pExp2)
                   pLisc = Pisc <$> pList  (pKey "/\\" *> pExp2)

-- The left factored version of difference:
   pExp2  :: Parser Token P_Expression
   pExp2   = pExp3 <??> (f <$> pKey "-" <*> pExp3)
             where f _ rExp lExp = PDif (lExp, rExp)

-- The left factored version of right- and left residuals:
   pExp3  :: Parser Token P_Expression
   pExp3  =  pExp4 <??> (f <$>  (pKey "\\" <|> pKey "/") <*> pExp4 )
             where f "\\" rExp lExp = PRrs (lExp, rExp)
                   f _    rExp lExp = PLrs (lExp, rExp)

-- composition and relational addition are associative, and parsed similar to union and intersect...
   pExp4  :: Parser Token P_Expression
   pExp4   = f <$> pExp5 <*> (pLrad <|> pLcps <|> pLprd)
             where f x (PCps []) = x
                   f x (PRad []) = x
                   f x (PPrd []) = x
                   f x (PCps xs) = PCps (x:xs)
                   f x (PRad xs) = PRad (x:xs)
                   f x (PPrd xs) = PPrd (x:xs)
                   f _ _ = fatal 301 "PRad PCps expected"
                   pLrad = PRad <$> pList1 (pKey "!" *> pExp5)
                   pLcps = PCps <$> pList1 (pKey ";" *> pExp5)
                   pLprd = PPrd <$> pList  (pKey "*" *> pExp5)

   pExp5  :: Parser Token P_Expression
   pExp5  =  f <$> pList (pKey "-") <*> pExp6  <*> pList ( pKey "~" <|> pKey "*" <|> pKey "+" )
             where f ms pe ("~":ps) = PFlp (f ms pe ps)
                   f ms pe ("*":ps) = PKl0 (f ms pe ps)
                   f ms pe ("+":ps) = PKl1 (f ms pe ps)
                   f ("-":ms) pe ps = PCpl (f ms pe ps)
                   f _ pe _         = pe

   pExp6  :: Parser Token P_Expression
   pExp6  =  f <$> pExp7 <*> optional pSign
             where f e Nothing = e
                   f e (Just sgn) = PTyp e sgn
-- Alternatively, this works too:    pExp6  =  pExp7 <??> (flip PTyp <$> pSign)

   pExp7  :: Parser Token P_Expression
   pExp7  =  Prel <$> pRelation                                                <|>
             PBrk <$  pSpec '('  <*>  pExpr  <*  pSpec ')'


   pRelation        :: Parser Token P_Relation
   pRelation         = P_I <$ pKey "I"      <|>
                       P_V <$ pKey "V"      <|>
                       rebuild <$> pVarid_val_pos      <|>
                       single  <$> pAtom 
                       where rebuild (nm,pos') = P_Rel {rel_nm = nm, rel_pos = pos'}
                             single x = P_Mp1 { rel_1val = x}     

   pRelSign         :: Parser Token (P_Relation, P_Sign)
   pRelSign          = f <$> pRelation <*> optional pSign
                        where f rel Nothing    = (rel,P_Sign [])
                              f rel (Just sgn) = (rel,sgn)

   pSign :: Parser Token P_Sign
   pSign = rebuild <$ pSpec '[' <*> pConcept <*> optional (pKey "*" *> pConcept) <* pSpec ']'
      where
        rebuild :: P_Concept -> Maybe P_Concept -> P_Sign
        rebuild a mb = case mb of 
                        Just b -> P_Sign { psign = [a,b] }
                        Nothing -> P_Sign { psign = [a] }
   
   pConcept         :: Parser Token P_Concept
   pConcept          = (P_Singleton <$ pKey "ONE") <|> (PCpt <$> (pConid <|> pString))
                      -- where c str = C str (==) []

-- BECAUSE:
--  (SJ) Waarom heeft een label (optioneel) strings?
--  (GM) Dit is bedoeld als binding mechanisme voor implementatiespecifieke (SQL/PHP plug,PHP web app,etc) properties
--  (SJ) Met het invoeren van referenties (t.b.v losse Explanations) bestaat er een variant met props en eentje zonder.
   pLabelProps      :: Parser Token Label
   pLabelProps       = lbl <$> pADLid_val_pos
                           <*> (pArgs `opt` [])
                           <*  pKey_pos ":"
                       where lbl :: (String, Origin) -> [[String]] -> Label
                             lbl (nm,pos')  = Lbl nm pos' 
                             pArgs = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid) <* pSpec '}'


   pADLid           :: Parser Token String
   pADLid            = pVarid <|> pConid <|> pString

   pADLid_val_pos   :: Parser Token (String, Origin)
   pADLid_val_pos    = pVarid_val_pos <|> pConid_val_pos <|> pString_val_pos

   pConceptDef      :: Parser Token ConceptDef
   pConceptDef       = Cd <$> pKey_pos "CONCEPT"
                          <*> (pConid <|> pString)   -- the concept name
                          <*> ((True <$ pKey "BYPLUG") `opt` False)
                          <*> pString                -- the definition text
                          <*> ((pKey "TYPE" *> pString) `opt` "")     -- the type of the concept.
                          <*> (pString `opt` "")     -- a reference to the source of this definition.


-- A key definition looks like:   KEY Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- You may also use an expression on each attribute place, for example: KEY onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
-- For the sake of a proper user interface, you can assign labels to the attributes in a key, for example:
-- KEY onSSN: Person("social security number":ssn)
   pKeyDef          :: Parser Token P_KeyDef
   pKeyDef           = kd <$ pKey "KEY" <*> pLabelProps <*> pConcept <* pSpec '(' <*> pList1Sep (pSpec ',') pKeyAtt <* pSpec ')'
                        where kd :: Label -> P_Concept -> P_ObjectDefs -> P_KeyDef 
                              kd (Lbl nm p _) c ats = P_Kd { kd_pos = p
                                                           , kd_lbl = nm
                                                           , kd_cpt = c
                                                           , kd_ats = [if null (obj_nm x) then x{obj_nm=show i} else x | (i,x)<-zip [(1::Integer)..] ats]
                                                           }

   pKeyAtt          :: Parser Token P_ObjectDef
   pKeyAtt           = attL <$> pLabelProps <*> pExpr <|>
                       att <$> pExpr
                       where attL (Lbl nm p strs) attexpr = 
                               P_Obj { obj_nm   = nm
                                     , obj_pos  = p
                                     , obj_ctx  = attexpr 
                                     , obj_ats  = []
                                     , obj_strs = strs
                                     }
                             att attexpr = 
                               P_Obj { obj_nm   = ""
                                     , obj_pos  = Origin "pKeyAtt CCv221.hs"
                                     , obj_ctx  = attexpr 
                                     , obj_ats  = []
                                     , obj_strs = []
                                     }

   pRoleRelation    :: Parser Token P_RoleRelation
   pRoleRelation      = rr <$> pKey_pos "ROLE"              <*>
                               pList1Sep (pSpec ',') pADLid <*
                               pKey "EDITS"                 <*>
                               pList1Sep (pSpec ',') pRelSign
                        where rr p roles rels = P_RR roles rels p

   pRoleRule        :: Parser Token RoleRule
   pRoleRule         = rr <$> pKey_pos "ROLE"               <*>
                              pList1Sep (pSpec ',') pADLid  <*
                              pKey "MAINTAINS"              <*>
                              pList1Sep (pSpec ',') pADLid 
                       where rr p r s = Maintain r s p

   pSqlplug         :: Parser Token P_ObjectDef
   pSqlplug          = pKey_pos "SQLPLUG" *> pObj

   pPhpplug         :: Parser Token P_ObjectDef
   pPhpplug          = pKey_pos "PHPPLUG" *> pObj

   pInterface       :: Parser Token P_Interface
   pInterface        = lbl <$> (pKey "INTERFACE" *> pADLid_val_pos) <*>
                               (pParams `opt` [])                   <*>  -- a list of relations, which are editable within this interface.
                               (pArgs `opt` [])                     <*>  -- a list of arguments for code generation.
                               (pRoles `opt` [])                    <*>  -- a list of roles that may use this interface
                               (pKey ":" *> pExpr)                  <*>  -- the context expression (mostly: I[c])
                               pAttrs                                  -- the subobjects
                       where lbl :: (String, Origin) -> [(P_Relation,P_Sign)] -> [[String]] -> [String] -> P_Expression -> [P_ObjectDef] -> P_Interface
                             lbl (nm,p) params args roles expr ats
                              = P_Ifc { ifc_Name   = nm
                                      , ifc_Params = params
                                      , ifc_Args   = args
                                      , ifc_Roles   = roles
                                      , ifc_Obj    = P_Obj { obj_nm   = nm    
                                                           , obj_pos  = p
                                                           , obj_ctx  = expr
                                                           , obj_ats  = ats
                                                           , obj_strs = args
                                                           }
                                      , ifc_Pos    = p
                                      , ifc_Expl   = ""
                                    }
                             pParams = pSpec '(' *> pList1Sep (pSpec ',') pRelSign          <* pSpec ')' 
                             pArgs   = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid)   <* pSpec '}'
                             pRoles  = pKey "FOR" *> pList1Sep (pSpec ',') pADLid
                             pAttrs  = pBox

   pObj             :: Parser Token P_ObjectDef
   pObj              = obj <$> pLabelProps
                           <*> pExpr                                             -- the context expression (for example: I[c])
                           <*> (pBox `opt` [])  -- the subobjects
                       where obj (Lbl nm pos' strs) expr ats = 
                               P_Obj { obj_nm   = nm
                                     , obj_pos  = pos'
                                     , obj_ctx  = expr
                                     , obj_ats  = ats
                                     , obj_strs = strs
                                     }
   pBox            :: Parser Token [P_ObjectDef]
   pBox            = pKey "BOX" *> pSpec '[' *> pList1Sep (pSpec ',') pObj <* pSpec ']'

   optional :: (Sequence p, Alternative p) => p a -> p (Maybe a)
   optional a        = Just <$> a <|> pSucceed Nothing


   pDeclaration     :: Parser Token P_Declaration
   pDeclaration      = ( rebuild <$> pVarid  <*> pKey_pos "::"  <*> pConcept  <*> (pKey "*" <|> pKey "->" )  <*> pConcept
                         <|>
                         rbd <$> pKey_pos "RELATION" <*> pVarid  <*> pSign
                       )
                         <*> ((True <$ pKey "BYPLUG") `opt` False)
                         <*> (pProps `opt` [])
                         <*> ((True <$ pKey "BYPLUG") `opt` False)
                         <*> (pPragma `opt` [])
                         <*> pList pMeaning
                         <*> ((pKey "=" *> pContent) `opt` [])
                         <* (pSpec '.' `opt` "")         -- in the syntax before 2011, a dot was required. This optional dot is there to save user irritation during the transition to a dotless era  :-) .
                       where rebuild nm pos' s fun' t bp1 props
                               = rbd pos' nm (P_Sign [s,t]) bp1 props'
                                 where props'= nub props `uni` if fun'=="->" then [Uni,Tot] else []
                             rbd pos' nm sgn bp1 props bp2 pragma meanings content
                               = P_Sgn { dec_nm   = nm
                                       , dec_sign = sgn
                                       , dec_prps = props
                                       , dec_prL  = head pr
                                       , dec_prM  = pr!!1
                                       , dec_prR  = pr!!2
                                       , dec_Mean = meanings
                                       , dec_popu = content
                                       , dec_fpos = pos'
                                       , dec_plug = bp1 || bp2
                                       }
                                 where pr = pragma++["","",""]

   pContent         :: Parser Token Pairs
   pContent          = pSpec '[' *> pListSep pComma pRecord <* pSpec ']'
                   <|> pSpec '[' *> pListSep (pKey ";") pRecordObs <* pSpec ']' --obsolete
       where
       pRecord = mkPair<$> pValue <* pKey "*" <*> pValue
       pValue  = pAtom <|> pConid <|> pVarid <|> pInteger <|> ((++)<$>pInteger<*>pConid) <|> ((++)<$>pInteger<*>pVarid)
       pRecordObs = mkPair<$ pSpec '(' <*> (trim <$> pString)  <* pComma   <*> (trim <$> pString)  <* pSpec ')' --obsolete

   -- | pProps is bedoeld voor gebruik in relatie-declaraties.
   pProps           :: Parser Token [Prop]
   pProps            = (f.concat) <$> (pSpec '[' *> pListSep (pSpec ',') pProp <* pSpec ']')
                       where f ps = nub (ps ++ concat [[Uni, Inj] | null ([Sym, Asy]>-ps)])

   pProp            :: Parser Token [Prop]
   pProp             = k [Uni] "UNI" <|> k [Inj] "INJ" <|> k [Sur] "SUR" <|> k [Tot] "TOT" <|>
                       k [Sym] "SYM" <|> k [Asy] "ASY" <|> k [Trn] "TRN" <|>
                       k [Rfx] "RFX" <|> k [Irf] "IRF" <|> k [Sym, Asy] "PROP"
                       where k obj str = f <$> pKey str where f _ = obj

   pPragma          :: Parser Token [String]
   pPragma           = pKey "PRAGMA" *> pList1 pString

   get_tok_pos :: Token -> Origin
   get_tok_pos     (Tok _ _ s l f) = FileLoc(FilePos (f,l,s))
   get_tok_val_pos :: Token -> (String, Origin)
   get_tok_val_pos (Tok _ _ s l f) = (s,FileLoc(FilePos (f,l,s)))



   gsym_pos :: IsParser p Token => TokenType -> String -> String -> p Origin
   gsym_pos kind val' val2' = get_tok_pos <$> pSym (Tok kind val' val2' noPos "")

   gsym_val_pos :: IsParser p Token => TokenType -> String -> String -> p (String,Origin)
   gsym_val_pos kind val' val2' = get_tok_val_pos <$> pSym (Tok kind val' val2' noPos "")

   pKey_pos :: String -> Parser Token Origin
   pKey_pos  keyword  =   gsym_pos TkKeyword   keyword   keyword

   pString_val_pos, {- pAtom_val_pos, -}pVarid_val_pos, pConid_val_pos
                      ::  IsParser p Token => p (String,Origin)
   pString_val_pos    =   gsym_val_pos TkString    ""        "?STR?"
   pVarid_val_pos     =   gsym_val_pos TkVarid     ""        "?LC?"
   pConid_val_pos     =   gsym_val_pos TkConid     ""        "?UC?"
   
