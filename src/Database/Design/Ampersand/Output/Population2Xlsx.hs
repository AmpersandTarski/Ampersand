{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.Population2Xlsx
  (fSpec2PopulationXlsx)
where
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.FSpec.FPA
import Database.Design.Ampersand.Basics
import System.Time

import Codec.Xlsx
import Control.Lens
import qualified Data.ByteString.Lazy as L
import System.Time

fSpec2PopulationXlsx :: ClockTime -> FSpec -> L.ByteString 
fSpec2PopulationXlsx ct fSpec = 
  fromXlsx ct xlsx
    where
      sheet = def & cellValueAt (1,2) ?~ CellDouble 42.0
                  & cellValueAt (3,2) ?~ CellText "foo"
      xlsx = def & atSheet "List1" ?~ sheet
     

