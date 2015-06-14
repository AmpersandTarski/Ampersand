{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Database.Design.Ampersand.Test.TestScripts (getTestScripts,testAmpersandScripts) where

import Data.List(sort,isInfixOf)
import Data.Char(toUpper)
import System.FilePath ((</>),takeExtension)
import Control.Monad (filterM, forM_, foldM,when)
import Control.Exception.Base
import System.IO.Error (tryIOError)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import Control.Monad.Trans.Class (lift)
import Data.Conduit
import Database.Design.Ampersand.Test.RunAmpersand (ampersand)
import Database.Design.Ampersand.Input.ADL1.CtxError

endswith :: String -> String -> Bool
endswith a b = drop (length a - length b) a == b

-- Returns tuple with files and subdirectories inside the given directory
getDirectory :: FilePath -> IO ([FilePath],[FilePath])
getDirectory path =
    do contents <- getDirectoryContents path
       let valid = filter (\x-> x /= "." && x /= "..") contents
       let paths = map (path ++) valid
       files <- filterM doesFileExist paths
       subdirs <- filterM doesDirectoryExist paths
       return (sort files, sort subdirs)

getFiles :: String -> FilePath -> IO [FilePath]
getFiles ext dir =
    do (fs, ds) <- getDirectory (dir++"/")
       let files = filter (`endswith` ext) fs
       foldM recursive files ds
      where recursive rs d =
                do ms <- getFiles ext d
                   return $ ms ++ rs

models' :: [FilePath]
models' = [ baseDir </> "Atlasv2/RepoRap/Fspec.adl"
         , baseDir </> "Bugs/Current/Other/Bug335_Kl0Kl1.adl"
         , baseDir </> "Bugs/Current/SQL/ARM20-Test8.adl"
         , baseDir </> "Bugs/Current/SQLFail/Bug331_TotalInjective.adl"
         , baseDir </> "Bugs/Current/SQLFail/Bug302_IgnoredV.adl"
         , baseDir </> "Bugs/Current MultiFile/Bug388_Retina/WBP-Glossary.adl"
         , baseDir </> "Bugs/Current MultiFile/Bug388_Retina/RETINABUG.adl"
         , baseDir </> "Bugs/Fixed/Other/Bug_GeneratedTables.adl"
         , baseDir </> "Bugs/Fixed/Other/Bug_ColumnProperties.adl"
         , baseDir </> "Bugs/Fixed/SQL/Bug_Totality.adl"
         , baseDir </> "Bugs/Fixed/SQL/Bug_SingletonExpression.adl"
         , baseDir </> "Bugs/Fixed/SQL/Bug_RelAdd.adl"
         , baseDir </> "Bugs/Fixed/SQL/Bug_HaskellViolationTest.adl"
         , baseDir </> "Bugs/No Sentinel/TechDataModelTest.adl"
         , baseDir </> "Bugs/No Sentinel/Bug434.adl"
         , baseDir </> "Bugs/No Sentinel/Bug337_FatalONE.adl"
         , baseDir </> "Delivery/IdentiSample.adl"
         , baseDir </> "Delivery/DeliverySessions.adl"
         , baseDir </> "Delivery/DeliveryRieksObl.adl"
         , baseDir </> "Delivery/DeliveryRieksNoObl.adl"
         , baseDir </> "Delivery/DeliveryAccounts.adl"
         , baseDir </> "Delivery/Delivery.adl"
         , baseDir </> "FPA/FPATest.adl"
         , baseDir </> "Generics/GenericYesNo.adl"
         , baseDir </> "Generics/GenericTransitiveClosure.adl"
         , baseDir </> "Generics/GenericSessionAccounts.adl"
         , baseDir </> "Generics/Generics.adl"
         , baseDir </> "Generics/GenericDateTime.adl"
         , baseDir </> "HRM/HRM Ontology.adl"
         , baseDir </> "INDOORS/overzichten/uitgangsscripts/uitgangslijstjes/wettelijkeTaken.adl"
         , baseDir </> "INDOORS/overzichten/uitgangsscripts/uitgangslijstjes/Producten.adl"
         , baseDir </> "INDOORS/overzichten/uitgangsscripts/uitgangslijstjes/Gebeurtenissen.adl"
         , baseDir </> "INDOORS/overzichten/uitgangsscripts/uitgangslijstjes/BusinessInterfaces.adl"
         , baseDir </> "INDOORS/overzichten/uitgangsscripts/versies/Taken.adl"
         , baseDir </> "INDOORS/overzichten/uitgangsscripts/versies/RfPtje.adl"
         , baseDir </> "INDOORS/overzichten/uitgangsscripts/wetsteksten.adl"
         , baseDir </> "INDOORS/overzichten/uitgangsscripts/VIROarch.adl"
         , baseDir </> "INDOORS/overzichten/uitgangsscripts/Populaties.adl"
         , baseDir </> "INDOORS/overzichten/uitgangsscripts/Handelingen.adl"
         , baseDir </> "INDOORS/Gebeurtenis.adl"
         , baseDir </> "INDOORS/DemoLamicieCode105.adl"
         , baseDir </> "Inspectie OCW/OCWdemo/Voorschriften.adl"
         , baseDir </> "ISTAR/ISTAR.adl"
         , baseDir </> "ISTAR/ISTAR-Workshop.adl"
         , baseDir </> "ISTAR/GenericDataTypes.adl"
         , baseDir </> "Misc/Kernmodel.adl"
         , baseDir </> "Misc/Hello.adl"
         , baseDir </> "Misc/FraakTest2.adl"
         , baseDir </> "Misc/FraakTest1.adl"
         , baseDir </> "Misc/ARM20-Test8.adl"
         , baseDir </> "Misc/ARM20-Test6.adl"
         , baseDir </> "Misc/ARM20-Test5.adl"
         , baseDir </> "Misc/ARM20-Test4.adl"
         , baseDir </> "Misc/ARM20-Test3.adl"
         , baseDir </> "Misc/ARM20-Test2.adl"
         , baseDir </> "Misc/ARM20-Test1.adl"
         , baseDir </> "Misc/ARM-Test2.adl"
         , baseDir </> "Misc/ARM-Test1.adl"
         , baseDir </> "Misc/ArchiTest5.adl"
         , baseDir </> "Misc/ArchiTest4.adl"
         , baseDir </> "Misc/ArchiTest3.adl"
         , baseDir </> "NVWA DTV/Periodeverantwoordingen.adl"
         , baseDir </> "NVWA DTV/OnregelmatigeDiensten.adl"
         , baseDir </> "NVWA DTV/Onderbrekingen.adl"
         , baseDir </> "NVWA DTV/Login.adl"
         , baseDir </> "NVWA DTV/expTijdsduur.adl"
         , baseDir </> "RAPRelatedPatterns/UIservices.adl"
         , baseDir </> "ServiceDesk/case.adl"
         , baseDir </> "Simple/DeliverySimpleOPA.adl"
         , baseDir </> "Simple/DeliverySimple.adl"
         , baseDir </> "Simple/Delivery.adl"
         , baseDir </> "Tests/NoSentinel/Multiplicities.adl"
         , baseDir </> "Tests/ShouldSucceed/try9.adl"
         , baseDir </> "Tests/ShouldSucceed/Try52.adl"
         , baseDir </> "Tests/ShouldSucceed/Try51.adl"
         , baseDir </> "Tests/ShouldSucceed/Try50.adl"
         , baseDir </> "Tests/ShouldSucceed/Try46.adl"
         , baseDir </> "Tests/ShouldSucceed/try45.adl"
         , baseDir </> "Tests/ShouldSucceed/try43a.adl"
         , baseDir </> "Tests/ShouldSucceed/try42.adl"
         , baseDir </> "Tests/ShouldSucceed/try41a.adl"
         , baseDir </> "Tests/ShouldSucceed/try41.adl"
         , baseDir </> "Tests/ShouldSucceed/try40.adl"
         , baseDir </> "Tests/ShouldSucceed/Try39.adl"
         , baseDir </> "Tests/ShouldSucceed/Try38.adl"
         , baseDir </> "Tests/ShouldSucceed/try37.adl"
         , baseDir </> "Tests/ShouldSucceed/Try35.adl"
         , baseDir </> "Tests/ShouldSucceed/Try34.adl"
         , baseDir </> "Tests/ShouldSucceed/Try33.adl"
         , baseDir </> "Tests/ShouldSucceed/Try32.adl"
         , baseDir </> "Tests/ShouldSucceed/try31.adl"
         , baseDir </> "Tests/ShouldSucceed/try30.adl"
         , baseDir </> "Tests/ShouldSucceed/try29.adl"
         , baseDir </> "Tests/ShouldSucceed/try28.adl"
         , baseDir </> "Tests/ShouldSucceed/try22.adl"
         , baseDir </> "Tests/ShouldSucceed/try21.adl"
         , baseDir </> "Tests/ShouldSucceed/try20.adl"
         , baseDir </> "Tests/ShouldSucceed/try19.adl"
         , baseDir </> "Tests/ShouldSucceed/try15.adl"
         , baseDir </> "Tests/ShouldSucceed/try14.adl"
         , baseDir </> "Tests/ShouldSucceed/try13b.adl"
         , baseDir </> "Tests/ShouldSucceed/try13.adl"
         , baseDir </> "Tests/ShouldSucceed/try12.adl"
         , baseDir </> "Tests/ShouldSucceed/try10.adl"
         , baseDir </> "Tests/ShouldSucceed/Ticket398.adl"
         , baseDir </> "Tests/ShouldSucceed/SelectExprTest.adl"
         , baseDir </> "Tests/ShouldSucceed/residutest.adl"
         , baseDir </> "Tests/ShouldSucceed/propertytests.adl"
         , baseDir </> "Tests/ShouldSucceed/InterfaceTest4.adl"
         , baseDir </> "Tests/ShouldSucceed/InterfaceTest1.adl"
         , baseDir </> "Tests/ShouldSucceed/FlipPHPBug.adl"
         , baseDir </> "Tests/ShouldSucceed/EscapeTest.adl"
         , baseDir </> "Tests/ShouldSucceed/ClassyInterfaces.adl"
         , baseDir </> "Tests/ShouldSucceed/CheckDanglingPurposes.adl"
         , baseDir </> "Tests/ShouldSucceed/Bug435.adl"
         , baseDir </> "Tests/ShouldSucceed/ASTbugje.adl"
         , baseDir </> "Tests/ShouldSucceed/ARM-Test12.adl"
         , baseDir </> "Tests/ShouldSucceed/Archimate1.adl"
         , baseDir </> "Ticket380/CSA_Op Ontology.adl"
         , baseDir </> "Ticket380/CSA_Ontology.adl"
         , baseDir </> "Ticket394/CSA_Op.adl"
         , baseDir </> "Ticket394/CSA_Op Ontology.adl"
         , baseDir </> "VIRO/strippedFsVIROENG.adl"
         , baseDir </> "VOGDemo/StrafbladKoppeling.adl"
         , baseDir </> "VOGDemo/Identificatiemiddelen.adl"
         , baseDir </> "VOGDemo/Handelsregister.adl"
         , baseDir </> "VOGDemo/GBA.adl"
         , baseDir </> "VOGDemo/DEMO_VOG.adl"
         , baseDir </> "VOGDemo/BasisDatatypes.adl"
         , baseDir </> "VOGDemo2/StrafbladKoppeling.adl"
         , baseDir </> "VOGDemo2/Identificatiemiddelen.adl"
         , baseDir </> "VOGDemo2/Handelsregister.adl"
         , baseDir </> "VOGDemo2/BasisDatatypes.adl"
         , baseDir </> "Webshop/Webshop.adl"
         ]
  where
    baseDir = ".." </> "ampersand-models"

getTestScripts :: IO [FilePath]
getTestScripts =
     do fs <- getFiles ".adl" "ArchitectureAndDesign"
        ss <- getFiles ".adl" $ ".." </> "ampersand-models" </> "Tests" </> "ShouldSucceed"
        ds <- getFiles ".adl" $ "AmpersandData" </> "FormalAmpersand"
        return $ fs ++ ss ++ ds -- ++ models



data DirContent = DirList [FilePath] [FilePath]
                | DirError IOError
data DirData = DirData FilePath DirContent

testAmpersandScripts :: IO ()
testAmpersandScripts
 = do 
    walk baseDir $$ myVisitor
 where
    baseDir = ".." </> "ampersand-models"

-- Produces directory data
walk :: FilePath -> Source IO DirData
walk path = do 
    result <- lift $ tryIOError listdir
    case result of
        Right dl
            -> case dl of 
                DirList subdirs _
                 -> do
                     yield (DirData path dl)
                     forM_ subdirs (walk . (path </>))
                DirError err 
                 -> yield (DirData path (DirError err))
        Left err
            -> yield (DirData path (DirError err))

  where
    listdir = do
        entries <- getDirectoryContents path >>= filterHidden
        subdirs <- filterM isDir entries
        files <- filterM isFile entries
        return $ DirList subdirs (filter isRelevant files)
        where 
            isFile entry = doesFileExist (path </> entry)
            isDir entry = doesDirectoryExist (path </> entry)
            filterHidden paths = return $ filter (not.isHidden) paths
            isRelevant f = map toUpper (takeExtension f) `elem` [".ADL"]  
            isHidden dir = head dir == '.'
            
-- Consume directories
myVisitor :: Sink DirData IO ()
myVisitor = addCleanup (\_ -> putStrLn "Finished.") $ loop 1
  where
    loop :: Int -> ConduitM DirData a IO ()
    loop n = do
        lift $ putStr $ ">> " ++ show n ++ ". "
        mr <- await
        case mr of
            Nothing     -> return ()
            Just r      -> lift (process r) >> loop (n + 1)
    process (DirData path (DirError err)) = do
        putStrLn $ "I've tried to look in " ++ path ++ "."
        putStrLn $ "    There was an error: "
        putStrLn $ "       " ++ show err

    process (DirData path (DirList dirs files)) = do
        putStrLn $ path ++ ". ("++ show (length dirs) ++ " directorie(s) and " ++ show (length files) ++ " relevant file(s):"
        forM_ files (runATest path) 
     
runATest :: FilePath -> FilePath -> IO()
runATest path file =
  catch (runATest' path file) showError
   where 
     showError :: SomeException -> IO()
     showError err
       = do putStrLn "***** ERROR: Fatal error was thrown: *****"
            putStrLn $ (path </> file)
            putStrLn $ show err
            putStrLn "******************************************"
        
runATest' :: FilePath -> FilePath -> IO()
runATest' path file = do
       [(_,errs)] <- ampersand [path </> file]
       putStrLn 
         ( file ++": "++
           case (shouldFail,errs) of
                  (False, []) -> "OK.  => Pass"
                  (False, _ ) -> "Fail => NOT PASSED:"
                  (True , []) -> "Ok.  => NOT PASSED"
                  (True , _ ) -> "Fail => Pass"
         )
       when (not shouldFail) $ mapM_ putStrLn (map showErr (take 1 errs))  --for now, only show the first error
    where shouldFail = "SHOULDFAIL" `isInfixOf` map toUpper (path </> file)
 
