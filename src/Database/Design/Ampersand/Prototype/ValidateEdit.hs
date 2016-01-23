module Database.Design.Ampersand.Prototype.ValidateEdit where

import Prelude hiding (putStr, putStrLn)
import Data.List
import Data.Maybe
import System.FilePath hiding (isValid)
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Prototype.PHP
import Database.Design.Ampersand.FSpec.SQL
import qualified Database.Design.Ampersand.Misc.Options as Opts
import Database.Design.Ampersand.Classes.ConceptStructure

tempDbName :: String
tempDbName = "ampersand_temporaryeditvalidationdb"

validateEditScript :: FSpec -> [Population] -> [Population] -> [Char] -> IO Bool
validateEditScript fSpec beforePops afterPops editScriptPath =
 do { mFileContents <- readUTF8File editScriptPath
    ; case mFileContents of
        Left err -> error $ "ERROR reading file " ++ editScriptPath ++ ":\n" ++ err
        Right editScript ->
         do { --putStrLn $ "Population before edit operations:\n" ++ show beforePops
            ; --putStrLn $ "Expected population after edit operations:\n" ++ show afterPops
            ; putStrLn $ "Edit script:\n" ++ editScript
            
            ; createTempDatabase fSpec beforePops
            ; let phpDir = Opts.dirPrototype (getOpts fSpec) </> "php"
            ; let phpScript = "ValidateEdit.php"
            ; putStrLn $ "Executing php script "++ phpDir </> phpScript
            ; _ <- executePHP (Just phpDir) phpScript [editScript] -- TODO: escape
            
            ; let expectedConceptTables  = [ (c,map showValSQL atoms) | ACptPopu c atoms <- afterPops ]
            ; let expectedRelationTables = [ (d,map showValsSQL pairs) | ARelPopu{popdcl=d,popps=pairs} <- afterPops ]
            ; let actualConcepts = [ c | c<- concs fSpec, c /= ONE, name c /= "SESSION" ] -- TODO: are these the right concepts and decls?
            ; let actualRelations = allDecls fSpec            --
            ; actualConceptTables <- mapM (getSqlConceptTable fSpec) actualConcepts
            ; actualRelationTables <- mapM (getSqlRelationTable fSpec) actualRelations
            ; let commonConcepts = getCommons expectedConceptTables actualConceptTables
            ; let commonRelations = getCommons expectedRelationTables actualRelationTables
            
            ; putStrLn $ "\n--- Validation results ---\n" 
            ; putStrLn $ "Actual concept tables:\n" ++ unlines [ name c ++ ": " ++ show atoms | (c,atoms) <- actualConceptTables ] 
            ; putStrLn $ "Actual relations:\n" ++ unlines [ name d ++ ": " ++ show pairs | (d,pairs) <- actualRelationTables ]

            ; putStrLn $ "Expected concept tables:\n" ++ unlines [ name c ++ ": " ++ show atoms | (c,atoms) <- expectedConceptTables ] 
            ; putStrLn $ "Expected relations:\n" ++ unlines [ name d ++ ": " ++ show pairs | (d,pairs) <- expectedRelationTables ] 

            ; let conceptDiffs  = showDiff "Actual population" "concepts" (map fst expectedConceptTables) (map fst actualConceptTables)
            ; let relationDiffs = showDiff "Actual population" "relations" (map fst expectedRelationTables) (map fst actualRelationTables)
            
            ; let commonConceptDiffs  = concat [ showDiff (name c) "atoms" expAtoms resAtoms | (c, expAtoms, resAtoms) <- commonConcepts ]
            ; let commonRelationDiffs = concat [ showDiff (name r) "pairs" expPairs resPairs | (r, expPairs, resPairs) <- commonRelations ]

            ; putStrLn $ "\n--- Validation summary ---\n" 
            
            ; if null conceptDiffs 
              then putStrLn "Expected and actual populations contain the same concepts"
              else putStrLn $ unlines $ conceptDiffs
            ; putStrLn ""
            ; if null relationDiffs 
              then putStrLn "Expected and actual populations contain the same relations"
              else putStrLn $ unlines $ relationDiffs
            ; putStrLn ""            
            ; if null commonConceptDiffs 
              then putStrLn "Common concepts are equal"
              else putStrLn $ unlines $ "Differences for common concepts:"  : commonConceptDiffs 
            ; putStrLn ""            
            ; if null commonRelationDiffs
              then putStrLn "Common relations are equal"
              else putStrLn $ unlines $ "Differences for common relations:" : commonRelationDiffs 
            
            ; let isValid = null $ conceptDiffs ++ relationDiffs ++ commonConceptDiffs ++ commonRelationDiffs
            ; putStrLn $ "\nValidation " ++ if isValid then "was successful." else "failed."
            ; return  isValid
            }
    }
  where showValsSQL p = ((showValSQL.apLeft) p, (showValSQL.apRight) p)
createTempDatabase :: FSpec -> [Population] -> IO ()
createTempDatabase fSpec pops =
 do { _ <- executePHPStr . showPHP $ sqlServerConnectPHP fSpec ++
                                     createTempDbPHP tempDbName ++
                                     createTablesPHP fSpec ++
--                                     [ "TODO: "
--                                     , "*** Beware: This script has bitrotted! ***"
--                                     , "To get it on her feet again, bsure not to forget"
--                                     , "to initialize the signal table too. "
--                                     ] ++
                                     populateTablesWithInitialPopsPHP fSpec
    ; return ()
    }

getSqlConceptTable :: FSpec -> A_Concept -> IO (A_Concept, [String])
getSqlConceptTable fSpec c =
 do { -- to prevent needing a unary query function, we add a dummy NULL column and use `src` and `tgt` as column names (in line with what performQuery expects)
      let query = case lookupCpt fSpec c of
                    []                      -> fatal 58  "No concept table for concept \"" ++ name c ++ "\""
                    (table,conceptAttribute):_ -> "SELECT DISTINCT `" ++ attName conceptAttribute ++ "` as `src`, NULL as `tgt`"++
                                                  " FROM `" ++ name table ++ "`" ++
                                                  " WHERE `" ++ attName conceptAttribute ++ "` IS NOT NULL"
    --; putStrLn $ "Query for concept " ++ name c ++ ":" ++ query 
    ; atomsDummies <- performQuery (getOpts fSpec) tempDbName query
    ; return (c, map fst atomsDummies)
    }

getSqlRelationTable :: FSpec -> Declaration -> IO (Declaration, [(String,String)])
getSqlRelationTable fSpec d =
 do { let query = prettySQLQuery fSpec 0 d
 
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

-- Utils

getCommons :: Eq a => [(a,bs)] -> [(a,bs)] -> [(a,bs,bs)]
getCommons elts1 elts2 = catMaybes
  [ case find (\(a',_)-> a' == a) elts2 of
      Just (_,bs2) -> Just (a, bs1, bs2)
      Nothing      -> Nothing 
  | (a,bs1) <- elts1 
  ]
  
showDiff :: (Eq a, Show a) => String -> String -> [a] -> [a] -> [String]
showDiff entityStr elementsStr expected actual =
  let unexpected = actual \\ expected
      missing    = expected \\ actual
  in  [ "!! " ++ entityStr ++ " is missing expected " ++ elementsStr ++ ": " ++ show missing | not . null $ missing ] ++
      [ "!! " ++ entityStr ++ " has unexpected " ++ elementsStr ++ ": " ++ show unexpected | not . null $ unexpected ]

