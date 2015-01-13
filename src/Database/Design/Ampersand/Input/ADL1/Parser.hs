{-# OPTIONS_GHC -fno-enable-rewrite-rules #-} -- Disable rewrite rules to drastically improve compilation speed
{-# LANGUAGE FlexibleContexts #-}
module Database.Design.Ampersand.Input.ADL1.Parser
  (AmpParser, pContext, pPopulations,pTerm, pRule, keywordstxt, keywordsops, specialchars, opchars) where

import Database.Design.Ampersand.Input.ADL1.Scanner
         ( Token(..),TokenType(..),Pos,noPos)
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim hiding (parse)
import Database.Design.Ampersand.Basics  (fatalMsg,Collection(..))
import Database.Design.Ampersand.Core.ParseTree
import Data.List
import Data.Maybe

type AmpParser a = Parser a -- GenParser Token Pos a
data Pair a r = Pair a r

fatal :: Int -> String -> a
fatal = fatalMsg "Parser"

keywordstxt :: [String]
keywordstxt       = [ "INCLUDE"
                    , "CONTEXT", "ENDCONTEXT", "EXTENDS", "THEMES"
                    , "META"
                    , "PATTERN", "ENDPATTERN"
                    , "PROCESS", "ENDPROCESS"
                    , "INTERFACE", "CLASS", "FOR", "BOX", "ROWS", "COLS", "INITIAL", "SQLPLUG", "PHPPLUG", "TYPE"
                    , "POPULATION", "CONTAINS"
                    , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX", "IRF", "AUT", "PROP", "ALWAYS"
                    , "RULE", "MESSAGE", "VIOLATION", "SRC", "TGT", "TEST"
                    , "RELATION", "MEANING", "CONCEPT", "IDENT"
                    , "VIEW", "TXT", "PRIMHTML"
                    , "KEY" -- HJO, 20130605: Obsolete. Only usefull as long as the old prototype generator is still in use.
                    , "IMPORT", "SPEC", "ISA", "IS", "I", "V"
                    , "CLASSIFY"
                    , "PRAGMA", "PURPOSE", "IN", "REF", "ENGLISH", "DUTCH"
                    , "REST", "HTML", "LATEX", "MARKDOWN"
                    , "ONE"
                    , "BYPLUG"
                    , "ROLE", "EDITS", "MAINTAINS"
                    ]

keywordsops :: [String]
keywordsops       = [ "|-", "-", "->", "<-", ">", "=", "~", "+", "*", ";", "!", "#", "::", ":", "\\/", "/\\", "\\", "/", "<>"
                    , "..", "." , "0", "1"]

specialchars :: String
specialchars      = "()[],{}"

opchars :: String
opchars           = nub (sort (concat keywordsops))

--- Populations ::= Population+
pPopulations :: AmpParser [P_Population]
pPopulations = return []

--- Context ::= 'CONTEXT' ConceptName LanguageRef TextMarkup? ContextElement* 'ENDCONTEXT'
pContext :: AmpParser (P_Context, [String]) -- the result is the parsed context and a list of include filenames
pContext = return (PCtx{}, [])
---     ContextElement ::= Meta | PatternDef | ProcessDef | RuleDef | Classify | RelationDef | ConceptDef | GenDef | Index | ViewDef | Interface | Sqlplug | Phpplug | Purpose | Population | PrintThemes | IncludeStatement

--- IncludeStatement ::= 'INCLUDE' String
--- LanguageRef ::= 'IN' ('DUTCH' | 'ENGLISH')
--- TextMarkup ::= 'REST' | 'HTML' | 'LATEX' | 'MARKDOWN'
--- Meta ::= 'META' String String
--- PatternDef ::= 'PATTERN' ConceptName PatElem* 'ENDPATTERN'
---     PatElem ::= RuleDef | Classify | RelationDef | ConceptDef | GenDef | Index | ViewDef | Purpose | Population
--- ProcessDef ::= 'PROCESS' ConceptName ProcElem* 'ENDPROCESS'
---     ProcElem ::= RuleDef | Classify | RelationDef | RoleRule | RoleRelation | ConceptDef | GenDef | Index | ViewDef | Purpose | Population
--- Classify ::= 'CLASSIFY' ConceptRef 'IS' Cterm
---                  Cterm ::= Cterm1 ('/\' Cterm1)*
---                  Cterm1 ::= ConceptRef | ('('? Cterm ')'?)
--- RuleDef ::= 'RULE' (ADLid ':'?)? Rule Meaning* Message* Violation?
pRule :: AmpParser (Term TermPrim)
pRule = return (Prim (PI OriginUnknown))
---                  Violation ::= 'VIOLATION' PairView
---                  PairView ::= '(' PairViewSegmentList ')'
---                  PairViewSegmentList  ::= PairViewSegment (',' PairViewSegment)*
---                  PairViewSegment ::= SrcOrTgt Term | 'TXT' String

--- SrcOrTgt ::= 'SRC' | 'TGT'

--- RelationDef ::= (Varid '::' ConceptRef Fun ConceptRef | 'RELATION' Varid Sign) 'BYPLUG'? Props? 'BYPLUG' Pragma? Meaning* ('=' Content)? '.'?
---                           Props ::= '[' PropList? ']'
---                           PropList ::= Prop (',' Prop)*
---                           Prop ::= 'UNI' | 'INJ' | 'SUR' | 'TOT' | 'SYM' | 'ASY' | 'TRN' | 'RFX' | 'IRF' | 'AUT' | 'PROP'
---                           Pragma ::= 'PRAGMA' String+
---                           Fun ::= '*' | '->' | '<-' | '[' Mult '-' Mult ']'
---                                 Mult ::= ('0' | '1') '..' ('1' | '*') | '*' | '1'

--- ConceptDef ::= 'CONCEPT' ConceptName 'BYPLUG'? String ('TYPE' String)? String?

--- GenDef ::= 'SPEC' ConceptRef 'ISA' ConceptRef | 'CLASSIFY' ConceptRef 'ISA' ConceptRef | Classify

--- Index ::= 'IDENT' Label ConceptRefPos '(' IndSegmentList ')'
---           IndSegmentList ::= IndSegment (',' IndSegment)
---           IndSegment ::= IndAtt
---           IndAtt ::= LabelProps Term | Term

--- ViewDef ::= ('VIEW' | 'KEY') LabelProps ConceptOneRefPos '(' ViewSegmentSepList ')'
---           ViewSegmentSepList ::= ViewSegment (',' ViewSegment)*
---           ViewSegment ::= ViewAtt | 'TXT' String | 'PRIMHTML' String
---           ViewAtt ::= LabelProps? Term

--- Interface ::= 'INTERFACE' ADLid 'CLASS'? (Conid | String) Params? InterfaceArgs? Roles? ':' Term SubInterface
---           Params ::= '(' RelSignList ')'
---           InterfaceArgs ::= '{' ADLidListList '}'
---           Roles ::= 'FOR' ADLidList

--- SubInterface ::= ('BOX' | 'ROWS' | 'COLS') Box | 'INTERFACE' ADLid

--- ObjDef ::= LabelProps Term SubInterface?

--- ObjDefList ::= ObjDef (',' ObjDef)*

--- Box ::= '[' ObjDefList ']'

--- Sqlplug ::= 'SQLPLUG' ObjDef

--- Phpplug ::= 'PHPPLUG' ObjDef

--- Purpose ::= 'PURPOSE' Ref2Obj LanguageRef? TextMarkup? ('REF' StringListSemi)? Expl
---        Ref2Obj ::= 'CONCEPT' ConceptName | 'RELATION' RelSign | 'RULE' ADLid | 'IDENT' ADLid | 'VIEW' ADLid | 'PATTERN' ADLid | 'PROCESS' ADLid | 'INTERFACE' ADLid | 'CONTEXT' ADLid

--- Population ::= 'POPULATION' RelSign 'CONTAINS' Content | 'POPULATION' ConceptName 'CONTAINS' '[' StringList ']'

--- RoleRelation ::= 'ROLE' ADLidList 'EDITS' RelSignList

--- RoleRule ::= 'ROLE' ADLidList 'MAINTAINS' ADLidList

--- PrintThemes ::= 'THEMES' ConceptNameList

--- Meaning ::= 'MEANING' LanguageRef? TextMarkup? (String | Expl)

--- Message ::= 'MESSAGE' LanguageRef? TextMarkup? (String | Expl)

--- Rule ::= Term ('=' Term | '|-' Term)?

--- Term ::= Trm2 (('\/' Trm2)* | ('\/' Trm2)*)?
pTerm :: AmpParser (Term TermPrim)
pTerm = pRule

--- Trm2 ::= Trm3 ('-' Trm3)?

--- Trm3 ::= Trm4 ('/' Trm4 | '\' Trm4 | '<>' Trm4)?

--- Trm4 ::= Trm5 ((';' Trm5)+ | ('!' Trm5)+ | ('#' Trm5)+)?

--- Trm5 ::= '-'* Trm6 ('~' | '*' | '+')*

--- Trm6 ::= Relation | '(' Term ')'

--- RelationRef ::= RelSign | 'I' ('[' ConceptOneRef ']')? | 'V' Sign? | Atom ('[' ConceptOneRef ']')?

--- RelSignList ::= RelSign (',' RelSign)*

--- RelSign ::= Varid Sign?

--- Sign ::= '[' ConceptOneRef ('*' ConceptOneRef)? ']'

--- ConceptName ::= Conid | String

--- ConceptNameList ::= ConceptName (',' ConceptName)

--- ConceptRef ::= ConceptName

--- ConceptOneRef ::= 'ONE' | ConceptRef

--- ConceptRefPos ::= Conid | String

--- ConceptOneRefPos ::= 'ONE' | Conid | String

--- LabelProps ::= ADLid LabelPropsArgs? ':'
---                           LabelPropsArgs ::= '{' ADLidListList '}'

--- Label ::= ADLid ':'

--- Content ::= '[' RecordList? ']' | '[' RecordObsList? ']'
---     RecordList ::= Record (',' Record)*
---     Record ::= String '*' String
---     RecordObsList ::= RecordObsList (';' RecordObsList)
---     RecordObs ::= '(' String ',' String ')'

--- ADLid ::= Varid | Conid | String

--- ADLidList ::= ADLid (',' ADLid)*

--- ADLidListList ::= ADLid+ (',' ADLid+)*
