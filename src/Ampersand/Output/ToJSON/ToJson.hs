{-# LANGUAGE RecordWildCards #-}
module Ampersand.Output.ToJSON.ToJson
  (generateJSONfiles
  ,generateJSONfilesRap)
where
import Ampersand.Output.ToJSON.JSONutils
import Ampersand.Output.ToJSON.Settings
import Ampersand.Output.ToJSON.Populations
import Ampersand.Output.ToJSON.Relations
import Ampersand.Output.ToJSON.Rules 
import Ampersand.Output.ToJSON.Concepts 
import Ampersand.Output.ToJSON.Conjuncts 
import Ampersand.Output.ToJSON.Interfaces 
import Ampersand.Output.ToJSON.Views
import Ampersand.Output.ToJSON.Roles

generateJSONfilesRap :: FSpec -> RIO App ()
generateJSONfilesRap fSpec = do
  env <- ask
  let opts = getOptions env
  sequence_ $
       [ writeJSONFile "metaPopulation" 
                                (fromAmpersand opts fSpec fSpec :: Populations)
       ]
generateJSONfiles :: FSpec -> RIO App ()
generateJSONfiles fSpec = do 
  env <- ask
  let opts = getOptions env
  sequence_ $
       [ writeJSONFile "settings"   (fromAmpersand opts fSpec fSpec :: Settings)
       , writeJSONFile "relations"  (fromAmpersand opts fSpec fSpec :: Relationz)
       , writeJSONFile "rules"      (fromAmpersand opts fSpec fSpec :: Rulez)
       , writeJSONFile "concepts"   (fromAmpersand opts fSpec fSpec :: Concepts)
       , writeJSONFile "conjuncts"  (fromAmpersand opts fSpec fSpec :: Conjuncts)
       , writeJSONFile "interfaces" (fromAmpersand opts fSpec fSpec :: Interfaces)
       , writeJSONFile "views"      (fromAmpersand opts fSpec fSpec :: Views)
       , writeJSONFile "roles"      (fromAmpersand opts fSpec fSpec :: Roles)
       , writeJSONFile "populations"(fromAmpersand opts fSpec fSpec :: Populations)
       ]


 



  
