{-# OPTIONS_GHC -Wall #-}
module Generators (generate)
where
import Control.Monad
import Options
import Data.Fspec   hiding (services)
import ShowHS                 (fSpec2Haskell)
import ShowADL
import XML.ShowXMLtiny        (showXML)
import Calc                   (deriveProofs)
import Prototype.ObjBinGen    (phpObjServices)
import Adl
import Fspec2Pandoc           (fSpec2Pandoc)
import Atlas.Atlas
import Picture
import Rendering.InfTree2Pandoc (proofdoc)
import Rendering.PandocAux (writepandoc)
import System.FilePath        (combine,replaceExtension)

generate :: Options -> Fspc -> IO ()
generate flags fSpec = 
    sequence_ 
       ([ verboseLn    flags "Generating..."]++
        [ doGenAtlas   fSpec flags | genAtlas     flags] ++
        [ doGenXML     fSpec flags | genXML       flags] ++
        [ doGenHaskell fSpec flags | haskell      flags] ++ 
        [ doGenProto   fSpec flags | genPrototype flags] ++
        [ serviceGen   fSpec flags | services     flags] ++
        [ doGenFspec   fSpec flags | genFspec     flags] ++ 
        [ prove        fSpec flags | proofs       flags] ++
        [ verbose flags "Done."]
       ) 

serviceGen :: Fspc -> Options -> IO()
serviceGen    fSpec flags
  = (writeFile outputFile $ showADLcode fSpec fSpec)
    >> verboseLn flags ("ADL written to " ++ outputFile ++ ".")
    where  outputFile = combine (dirOutput flags) "Generated.adl"

prove :: Fspc -> Options -> IO()
prove fSpec flags
    = putStr (deriveProofs flags fSpec)

doGenHaskell :: Fspc -> Options -> IO()
doGenHaskell fSpec flags
   =  verboseLn flags ("Generating Haskell source code for "++name fSpec)
   >> writeFile outputFile (fSpec2Haskell fSpec flags) 
   >> verboseLn flags ("Haskell written into " ++ outputFile ++ ".")
   where outputFile
           = combine (dirOutput flags) (replaceExtension (baseName flags) ".hs")


doGenAtlas :: Fspc -> Options -> IO()
doGenAtlas fSpec flags =
     verboseLn flags "Generating Atlas ..."
  >> verboseLn flags ("The atlas application should have been installed in " ++ show (dirAtlas flags) ++ ".")
  >> fillAtlas fSpec flags
   
doGenXML :: Fspc -> Options -> IO()
doGenXML fSpec flags 
   =  verboseLn flags "Generating XML..." >>
      writeFile outputFile ( showXML fSpec (genTime flags))   
   >> verboseLn flags ("XML written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".xml")
               
doGenProto :: Fspc -> Options -> IO()
doGenProto fSpec flags
   =  verboseLn flags "Checking on rule violations..."
     >> if (not.null) (violations fSpec) 
        then verboseLn flags explainviols else verboseLn flags "No violations found." 
     >> verboseLn flags "Generating prototype..."
     >> phpObjServices fSpec flags  
     >> verboseLn flags ("Prototype files have been written to " ++  (dirPrototype flags) ++ "." )
     >> if (test flags) then verboseLn flags (show (vplugs fSpec)) else verboseLn flags ""
     where 
     explainviols = concat [show p++": "++showADLcode fSpec r++"\n"|(r,p)<-violations fSpec]


-- This function will generate all Pictures for a given Fspc. 
-- the returned Fspc contains the details about the Pictures, so they
-- can be referenced while rendering the Fspc.
-- This function used to generate just func specs, but it was actually suitable to generate any pandoc document with pictures from an fSpec.
-- The option "theme" defines the content or type of document
-- current themes are: 
--    proofs -> a document with type inference proofs
--    student -> an adjusted func spec for students of the business rules course
--    <no theme> -> just the func spec
doGenFspec :: Fspc -> Options -> IO()
doGenFspec fSpec flags
   = verboseLn flags ("Processing "++name fSpec++" towards "++outputFile)     >>
     makeOutput                                                               >>
     verboseLn flags ("Document has been written into " ++ outputFile ++ ".") >>
     when (genGraphics flags && not(null thePictures)) 
          (foldr1 (>>) [ writePicture flags p| p<-thePictures] )              >>
     -- postProcessing of the generated output file depends on the format:
     postProcessor
     
       where
       (thePandoc,thePictures) = case theme flags of
              ProofTheme -> (proofdoc fSpec,[]) --generate a proof document
              _ -> fSpec2Pandoc fSpec flags --generate a func spec
       (outputFile,makeOutput,postProcessor) = writepandoc flags thePandoc

              
