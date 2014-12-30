module Database.Design.Ampersand.Prototype.ValidateEdit where

import Prelude hiding (putStr, putStrLn)
import Data.List
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.Prototype.PHP
import Database.Design.Ampersand.Prototype.RelBinGenSQL
import qualified Database.Design.Ampersand.Misc.Options as Opts

fatal :: Int -> String -> a
fatal = fatalMsg "Prototype.ValidateEdit"


tempDbName :: String
tempDbName = "ampersand_temporaryeditvalidationdb"

validateEditScript :: FSpec -> [Population] -> [Population] -> [Char] -> IO Bool
validateEditScript fSpec beforePops afterPops editScriptPath =
 do { mFileContents <- readUTF8File editScriptPath
    ; case mFileContents of
        Left err -> error $ "ERROR reading file " ++ editScriptPath ++ ":\n" ++ err
        Right editScript ->
         do { putStrLn $ "Population before edit operations:\n" ++ show beforePops
            ; -- putStrLn $ "Expected population after edit operations:\n" ++ show afterPops
            ; putStrLn $ "Edit script:\n" ++ editScript
            
            ; createTempDatabase fSpec beforePops
            ; phpOutput <- executePHP (Just $ Opts.dirPrototype (getOpts fSpec)) "php/ValidateEdit.php" 
                             [editScript] -- TODO: escape
            ; putStrLn $ phpOutput 
            
            ; let expectedConceptTables  = [ (c,atoms) | PCptPopu c atoms <- afterPops ]
            ; let expectedRelationTables = [ (d,pairs) | PRelPopu d pairs <- afterPops ]
            
            ; let concepts = allConcepts fSpec \\ [ONE]
            ; let relations = allDecls fSpec
            ; resultingConceptTables <- mapM (getSqlConceptTable fSpec) concepts
            ; resultingRelationTables <- mapM (getSqlRelationTable fSpec) relations
            
            ; putStrLn $ "Resulting concept tables:\n" ++ unlines [ name c ++ ": " ++ show atoms | (c,atoms) <- resultingConceptTables ] 
            ; putStrLn $ "Resulting relations:\n" ++ unlines [ name d ++ ": " ++ show pairs | (d,pairs) <- resultingRelationTables ]

            ; putStrLn $ "Expected concept tables:\n" ++ unlines [ name c ++ ": " ++ show atoms | (c,atoms) <- expectedConceptTables ] 
            ; putStrLn $ "Expected relations:\n" ++ unlines [ name d ++ ": " ++ show pairs | (d,pairs) <- expectedRelationTables ] 

            ; return True
            }
    }
    
createTempDatabase :: FSpec -> [Population] -> IO ()
createTempDatabase fSpec pops =
 do { _ <- executePHPStr . showPHP $ sqlServerConnectPHP fSpec ++
                                     createTempDbPHP tempDbName ++
                                     createTablesPHP fSpec ++
                                     populateTablesWithPopsPHP fSpec pops
    ; return ()
    }

getSqlConceptTable :: FSpec -> A_Concept -> IO (A_Concept, [String])
getSqlConceptTable fSpec c =
 do { -- to prevent needing a unary query function, we add a dummy NULL column and use `src` and `tgt` as column names (in line with what performQuery expects)
      let query = case lookupCpt fSpec c of
                    []                      -> fatal 58  "No concept table for concept \"" ++ name c ++ "\""
                    (table,conceptField):_ -> "SELECT DISTINCT `" ++ fldname conceptField ++ "` as `src`, NULL as `tgt`"++
                                              " FROM `" ++ name table ++ "`" ++
                                              " WHERE `" ++ fldname conceptField ++ "` IS NOT NULL"
    ; putStrLn $ "Query for concept " ++ name c ++ ":" ++ query 
    ; atomsDummies <- performQuery (getOpts fSpec) tempDbName query
    ; return (c, map fst atomsDummies)
    }

getSqlRelationTable :: FSpec -> Declaration -> IO (Declaration, [(String, String)])
getSqlRelationTable fSpec d =
 do { let query = selectExprRelation fSpec (-1) "src" "tgt" d
 
    --; putStrLn $ "Query for decl " ++ name d ++ ":" ++ query 
    ; pairs <- performQuery (getOpts fSpec) tempDbName query
    ; return (d, pairs)
    }
-- TODO: are we going to use this data type?

type EditScript = [SQLEditOp]

data SQLEditOp = SQLAddToConcept { atomNm :: String, conceptNm :: String }
               | SQLDelete { relationNm :: String, relationIsFlipped :: Bool
                           , parentAtomNm :: String, childAtomNm :: String }
               | SQLUpdate { relationNm :: String, relationIsFlipped :: Bool
                           , parentAtomNm :: String, parentConceptNm ::String
                           , childAtomNm :: String, childConceptNm ::String
                           , parentOrChild :: ParentOrChild, originalAtomNm :: String 
                           }

data ParentOrChild = Parent | Child deriving Show

{- JSON for edit commands from Database.PHP:

 { dbCmd: 'addToConcept', atom:atom, concept:concept }

 { dbCmd: 'update', relation:relation, isFlipped:relationIsFlipped
 , parentAtom:parentAtom, parentConcept:parentConcept
 , childAtom:childAtom, childConcept:childConcept
 , parentOrChild:parentOrChild, originalAtom:originalAtom
 }

 { dbCmd: 'delete', relation:relation, isFlipped:relationIsFlipped
 , parentAtom:parentAtom, childAtom:childAtom
 } 
 
-}
