{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.FSpec2Solidity (fSpec2Solidity)
where
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec
import Data.List as List
import Data.Monoid
import Data.Text as Text

fSpec2Solidity :: MultiFSpecs -> Text.Text
fSpec2Solidity multi =
  Text.intercalate "\n" $
         ["/*"
         ,"This file is generated using " <> (Text.pack ampersandVersionStr)
         ,"*/"
         ,"contract "<> Text.pack (name fSpec) <>" {"
         ,""
         ,"// PRESENT IN EVERY CONTRACT"
         ,""
         ,"  struct Concept {"
         ,"    bytes32 name; // name of the concept"
         ,"    bytes32[] ids; // id's of all the atoms in the concept"
         ,"  }"
         ,""
         ,"  struct Relation {"
         ,"    bytes32 name;"
         ,"    bool univalent;"
         ,"    bool total;"
         ,"    bool surjective;"
         ,"    bool injective;"
         ,"    bool reflexive;"
         ,"    bool irreflexive;"
         ,"    bool transitive;"
         ,"    bool symmetric;"
         ,"    bool asymmetric;"
         ,"    bool property;"
         ,"    Concept source;"
         ,"    Concept target;"
         ,"    mapping (bytes32 => bytes32) relation;"
         ,"    mapping (bytes32 => bytes32) relationFlip;"
         ,"  }"
         ,""
         ,"    // UNTESTED"
         ,"  function implies(bytes32 sourceAtoms, Relation storage a, Relation storage b, bool flip) internal returns(bool){"
         ,"    var leftMapping = flip ? a.relation : a.relationFlip;"
         ,"    var rightMapping = flip ? b.relation: b.relationFlip;"
         ,"    for(uint i = 0; i < sourceAtoms.length; ++i) {"
         ,"        var sourceAtom = sourceAtoms[i];"
         ,"        var targetAtomsLeft = leftMapping[sourceAtom];"
         ,"        var targetAtomsRight = rightMapping[sourceAtom];"
         ,"        for(uint j = 0; j < targetAtomsLeft.length; ++j) {"
         ,"            var targetAtomLeft = targetAtomsLeft[j];"
         ,"            var matched = false;"
         ,"            for (uint k = 0; k < targetAtomsRight.length; ++k) {"
         ,"                if (targetAtomsRight[k] == targetAtomLeft) {"
         ,"                    matched = true;"
         ,"                    continue;"
         ,"                }"
         ,"            }"
         ,"            if (!matched) {"
         ,"                return false;"
         ,"            }"
         ,"        }"
         ,"    }"
         ,"    return true;"
         ,"  }"
         ,""
         ,"// PRESENT BASED ON COMPILATION FROM ADL"
         ,""
         ,"// CONCEPTS:"
         ]
       <>(mconcat . fmap showConcept . allConcepts $ fSpec)
       <>[""
         ,"// RELATIONS:"]
       <>(mconcat . fmap showDecl . vrels $ fSpec)
       
   where
     fSpec = userFSpec multi
     showConcept :: A_Concept -> [Text.Text]
     showConcept cpt 
       = fmap ("  " <>)
          [""
          ,"// Concept " <> (Text.pack$ showA cpt)
          ,"Concept " <> uniqueId fSpec cpt <> " = Concept({"
          ,"  name: "<> Text.pack (name cpt)
          ,"  ids: new bytes32[](0)"
          ,"});"
          ]
     showDecl :: Declaration -> [Text.Text]
     showDecl decl 
        = [""
          ,"// " <> (Text.pack$ showA decl)
          ,"Relation " <> uniqueId fSpec decl <> " = Relation({"
          ,"  name: " <> (Text.pack . show . name $ decl)<> ","
          ,"  univalent: "   <> showProp isUni  <> ","
          ,"  total: "       <> showProp isTot  <> ","
          ,"  surjective: "  <> showProp isSur  <> ","
          ,"  injective: "   <> showProp isInj  <> ","
          ,"  reflexive: "   <> showProp isRfx  <> ","
          ,"  irreflexive: " <> showProp isIrf  <> ","
          ,"  transitive: "  <> showProp isTrn  <> ","
          ,"  symmetric: "   <> showProp isSym  <> ","
          ,"  asymmetric: "  <> showProp isAsy  <> ","
          ,"  property: "    <> showProp isProp <> ","
          ,"  source: "      <> (Text.pack . showA . source $ decl) <> ","
          ,"  target: "      <> (Text.pack . showA . target $ decl) <> ","
          ,"});"
          ] 
       where showProp :: (Declaration -> Bool) -> Text.Text
             showProp p = Text.toLower . Text.pack . show . p $ decl   

class UniqueId a where
  uniqueId :: FSpec -> a -> Text
instance UniqueId A_Concept where
  uniqueId _ cpt = Text.pack (name cpt)   
instance UniqueId Declaration where
  uniqueId fSpec decl = 
    case Prelude.filter (\x -> name x == name decl) (vrels fSpec) of
      []  -> fatal 97 $ "The declaration must exist in the fSpec" <> showA decl
      [_] -> Text.pack (name decl)
      xs  -> case List.elemIndex decl xs of
              Nothing -> fatal 102 $ "decl should be in this list of decls with the same name!" <> showA decl
              Just i  -> Text.pack (name decl ++ show i)