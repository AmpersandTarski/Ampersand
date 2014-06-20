{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Misc.TinyXML where

   -----------------some new data types for simple XML structures--------
   data XTree = Elem { etag :: XTag
                     , etrees :: [XTree]
                     }
              | Node { ntag :: XTag  -- WHY? is Node nodig? Immers een Elem met een lege etrees doet precies hetzelfde...
                                      -- BECAUSE! De show van een Node laat maar één tag zien. Een Elem showt een begin- en end-tag.
                     }
              | PlainText {ptstr :: String}
   data XTag =  Tag  { tName :: String
                     , tAtts :: [XAtt]
                     }
   data XAtt = Att { attName :: String
                   , attValue :: String
                   }


   showXTree :: XTree -> String
   showXTree tree = case tree of
                        Elem{} -> showStart tag
                               ++ concatMap showXTree (etrees tree)
                               ++ showEnd tag
                                   where tag = etag tree
                        Node{} -> showNode (ntag tree)
                        PlainText{} -> show (ptstr tree)
   showStart :: XTag -> String
   showStart a = "<" ++ tName a ++ showAtts (tAtts a) ++ ">" 
  
   showAtts :: [XAtt] -> String
   showAtts = concatMap showAtt
      where showAtt :: XAtt -> String
            showAtt a= " "++attName a++"="++show (attValue a)

   showEnd :: XTag -> String
   showEnd a = "</" ++ tName a ++ ">"   
   
   showNode :: XTag -> String
   showNode a = "<" ++ tName a ++ showAtts (tAtts a) ++ "/>"   

   mkAttr :: String -> String -> XAtt
   mkAttr  = Att 
   
   simpleTag :: String -> XTag
   simpleTag nm = Tag nm []
   
