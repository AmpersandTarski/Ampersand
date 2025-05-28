module Ampersand.Output.FSpec2Turtle (fSpec2Turtle) where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec
import Data.RDF
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import RIO.Text (Text)
import qualified RIO.Text as T

fSpec2Turtle :: FSpec -> Text
fSpec2Turtle = graph2Turtle . fSpec2Graph

fSpec2Graph :: FSpec -> RDF TList
fSpec2Graph fSpec = empty

graph2Turtle :: RDF TList -> Text
graph2Turtle rdf =
  "To be implemented: Turtle script for RDF graph\n"