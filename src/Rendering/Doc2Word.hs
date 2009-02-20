{-# OPTIONS_GHC -Wall #-}
module Rendering.Doc2Word where
   import Rendering.Document
   import Languages
   import Options
   import Version
   class ToWord a where
     render2Word :: Options -> a -> String

     
   instance ToWord Document where
      render2Word flags x = startCode 
                         ++ foldr (++) [] (map (render2Word flags) (dcont x))
                         ++ endCode
        where 
           startCode :: String
           startCode  
              = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><?mso-application progid=\"Word.Document\"?><w:wordDocument xmlns:w=\"http://schemas.microsoft.com/office/word/2003/wordml\" xmlns:v=\"urn:schemas-microsoft-com:vml\" xmlns:w10=\"urn:schemas-microsoft-com:office:word\" xmlns:sl=\"http://schemas.microsoft.com/schemaLibrary/2003/core\" xmlns:aml=\"http://schemas.microsoft.com/aml/2001/core\" xmlns:wx=\"http://schemas.microsoft.com/office/word/2003/auxHint\" xmlns:o=\"urn:schemas-microsoft-com:office:office\" xmlns:dt=\"uuid:C2F41010-65B3-11d1-A29F-00AA00C14882\" xmlns:wsp=\"http://schemas.microsoft.com/office/word/2003/wordml/sp2\" w:macrosPresent=\"no\" w:embeddedObjPresent=\"no\" w:ocxPresent=\"no\" xml:space=\"preserve\"><w:ignoreElements w:val=\"http://schemas.microsoft.com/office/word/2003/wordml/sp2\"/>\n"
              ++"<o:DocumentProperties><o:Title>"++docTitle flags++"</o:Title><o:Description>"++"Generated with "++versionbanner++"</o:Description></o:DocumentProperties>"
              ++"<w:fonts><w:defaultFonts w:ascii=\"Times New Roman\" w:fareast=\"Times New Roman\" w:h-ansi=\"Times New Roman\" w:cs=\"Times New Roman\"/></w:fonts><w:styles><w:versionOfBuiltInStylenames w:val=\"4\"/><w:latentStyles w:defLockedState=\"off\" w:latentStyleCount=\"156\"/><w:style w:type=\"paragraph\" w:default=\"on\" w:styleId=\"Normal\"><w:name w:val=\"Normal\"/><w:rPr><wx:font wx:val=\"Times New Roman\"/><w:sz w:val=\"24\"/><w:sz-cs w:val=\"24\"/><w:lang w:val=\"EN-US\" w:fareast=\"EN-US\" w:bidi=\"AR-SA\"/></w:rPr></w:style><w:style w:type=\"paragraph\" w:styleId=\"Heading1\"><w:name w:val=\"heading 1\"/><wx:uiName wx:val=\"Heading 1\"/><w:basedOn w:val=\"Normal\"/><w:next w:val=\"Normal\"/><w:rsid w:val=\"001A6AA1\"/><w:pPr><w:pStyle w:val=\"Heading1\"/><w:keepNext/><w:spacing w:before=\"240\" w:after=\"60\"/><w:outlineLvl w:val=\"0\"/></w:pPr><w:rPr><w:rFonts w:ascii=\"Arial\" w:h-ansi=\"Arial\" w:cs=\"Arial\"/><wx:font wx:val=\"Arial\"/><w:b/><w:b-cs/><w:kern w:val=\"32\"/><w:sz w:val=\"32\"/><w:sz-cs w:val=\"32\"/></w:rPr></w:style><w:style w:type=\"paragraph\" w:styleId=\"Heading2\"><w:name w:val=\"heading 2\"/><wx:uiName wx:val=\"Heading 2\"/><w:basedOn w:val=\"Normal\"/><w:next w:val=\"Normal\"/><w:rsid w:val=\"001A6AA1\"/><w:pPr><w:pStyle w:val=\"Heading2\"/><w:keepNext/><w:spacing w:before=\"240\" w:after=\"60\"/><w:outlineLvl w:val=\"1\"/></w:pPr><w:rPr><w:rFonts w:ascii=\"Arial\" w:h-ansi=\"Arial\" w:cs=\"Arial\"/><wx:font wx:val=\"Arial\"/><w:b/><w:b-cs/><w:i/><w:i-cs/><w:sz w:val=\"28\"/><w:sz-cs w:val=\"28\"/></w:rPr></w:style><w:style w:type=\"paragraph\" w:styleId=\"Heading3\"><w:name w:val=\"heading 3\"/><wx:uiName wx:val=\"Heading 3\"/><w:basedOn w:val=\"Normal\"/><w:next w:val=\"Normal\"/><w:rsid w:val=\"001A6AA1\"/><w:pPr><w:pStyle w:val=\"Heading3\"/><w:keepNext/><w:spacing w:before=\"240\" w:after=\"60\"/><w:outlineLvl w:val=\"2\"/></w:pPr><w:rPr><w:rFonts w:ascii=\"Arial\" w:h-ansi=\"Arial\" w:cs=\"Arial\"/><wx:font wx:val=\"Arial\"/><w:b/><w:b-cs/><w:sz w:val=\"26\"/><w:sz-cs w:val=\"26\"/></w:rPr></w:style><w:style w:type=\"character\" w:default=\"on\" w:styleId=\"DefaultParagraphFont\"><w:name w:val=\"Default Paragraph Font\"/><w:semiHidden/></w:style><w:style w:type=\"table\" w:default=\"on\" w:styleId=\"TableNormal\"><w:name w:val=\"Normal Table\"/><wx:uiName wx:val=\"Table Normal\"/><w:semiHidden/><w:rPr><wx:font wx:val=\"Times New Roman\"/></w:rPr><w:tblPr><w:tblInd w:w=\"0\" w:type=\"dxa\"/><w:tblCellMar><w:top w:w=\"0\" w:type=\"dxa\"/><w:left w:w=\"108\" w:type=\"dxa\"/><w:bottom w:w=\"0\" w:type=\"dxa\"/><w:right w:w=\"108\" w:type=\"dxa\"/></w:tblCellMar></w:tblPr></w:style><w:style w:type=\"list\" w:default=\"on\" w:styleId=\"NoList\"><w:name w:val=\"No List\"/><w:semiHidden/></w:style></w:styles><w:docPr><w:view w:val=\"print\"/><w:zoom w:percent=\"100\"/><w:doNotEmbedSystemFonts/><w:proofState w:spelling=\"clean\" w:grammar=\"clean\"/><w:attachedTemplate w:val=\"\"/><w:defaultTabStop w:val=\"720\"/><w:punctuationKerning/><w:characterSpacingControl w:val=\"DontCompress\"/><w:optimizeForBrowser/><w:validateAgainstSchema/><w:saveInvalidXML w:val=\"off\"/><w:ignoreMixedContent w:val=\"off\"/><w:alwaysShowPlaceholderText w:val=\"off\"/><w:compat><w:breakWrappedTables/><w:snapToGridInCell/><w:wrapTextWithPunct/><w:useAsianBreakRules/><w:dontGrowAutofit/></w:compat><wsp:rsids><wsp:rsidRoot wsp:val=\"001A6AA1\"/><wsp:rsid wsp:val=\"001A6AA1\"/><wsp:rsid wsp:val=\"004F3E2F\"/><wsp:rsid wsp:val=\"005F64C9\"/><wsp:rsid wsp:val=\"008A6C40\"/></wsp:rsids></w:docPr>\n"
              ++"<w:body><wx:sect>"
           endCode :: String
           endCode = "</wx:sect></w:body></w:wordDocument>"      

          
   instance ToWord DSContent where
      render2Word flags x =
           case x of          
             List{} -> undefined    --for the time being
             Par {} -> "<w:p wsp:rsidR=\"001A6AA1\" wsp:rsidRDefault=\"001A6AA1\" wsp:rsidP=\"001A6AA1\">"
                    ++    foldr (++) [] (map (render2Word flags) (dtxts x))
                    ++ "</w:p>"
             Section{} -> "<wx:sub-section>"
                       ++    "<w:p wsp:rsidR=\"001A6AA1\" wsp:rsidRDefault=\"001A6AA1\" wsp:rsidP=\"001A6AA1\">"   
                       ++    "<w:pPr>"
                       ++       "<w:pStyle w:val=\"Heading"++sectLevel++"\"/>"
                       ++       "<w:rPr>"
                       ++          "<w:lang w:val=\""++langCode flags++"\"/>"
                       ++       "</w:rPr>"
                       ++    "</w:pPr>"
                       ++    "<w:r>"
                       ++      "<w:rPr>"
                       ++        "<w:lang w:val=\""++langCode flags++"\"/>"
                       ++      "</w:rPr>"
                       ++      "<w:t>"++sectHeader++"</w:t>"
                       ++    "</w:r>"
                       ++    "</w:p>"
                       ++    foldr (++) [] (map (render2Word flags) (dscnts x))
                       ++ "</wx:sub-section>"
              where
                 sectHeader =  txt(dshead x)
                 sectLevel = show (dsLevel x)           


   instance ToWord DLItem where
       render2Word flags item = "\n\\item " ++ render2Word flags (liDTxt item)
        
   instance ToWord DTxt where
     render2Word flags dtxt
       = case dtxt of 
                 Text str -> "<w:r>"
                          ++ "  <w:rPr>"
                          ++ "     <w:lang w:val=\""++langCode flags++"\"/>"
                          ++ "  </w:rPr>"
                          ++ "  <w:t>"++str++"</w:t>"
                          ++ "</w:r>"
                 RefSection ref -> "<w:r>"
                          ++ "  <w:rPr>"
                          ++ "     <w:lang w:val=\""++langCode flags++"\"/>"
                          ++ "  </w:rPr>"
                          ++ "  <w:t>"++"#WERWIJZING NAAR: "++ref++"</w:t>"
                          ++ "</w:r>"
   langCode :: Options -> String
   langCode flags = case language flags of
                       Dutch -> "NL"               
                       English -> "UK"  --gegokt... uitzoeken of het werkt TODO



   effe :: String
   effe = ""
       ++     "<wx:sub-section>"
       ++         "<w:p wsp:rsidR=\"001A6AA1\" wsp:rsidRDefault=\"001A6AA1\" wsp:rsidP=\"001A6AA1\">"
  --     ++         "   <w:pPr>"
  --     ++         "     <w:pStyle w:val=\"Heading1\"/>"
  --     ++         "     <w:rPr>"
  --     ++         "       <w:lang w:val=\"NL\"/>"
  --     ++         "     </w:rPr>"
  --     ++         "   </w:pPr>"
       ++         "   <w:r>"
       ++         "     <w:rPr>"
       ++         "       <w:lang w:val=\"NL\"/>"
       ++         "     </w:rPr>"
       ++         "     <w:t>Hoofdstuk lvl 1</w:t>"
       ++         "   </w:r>"
       ++         "   <w:r>"
       ++         "      <w:rPr>"
       ++         "         <w:lang w:val=\"NL\"/>"
       ++         "      </w:rPr>"
       ++         "      <w:t>AAP</w:t>"
       ++         "   </w:r>"
       ++         "   <w:r><w:rPr><w:lang w:val=\"NL\"/></w:rPr><w:t> 1</w:t></w:r>"
       ++         "</w:p>"
       ++         "<w:p wsp:rsidR=\"001A6AA1\" wsp:rsidRDefault=\"001A6AA1\" wsp:rsidP=\"001A6AA1\"><w:pPr><w:rPr><w:lang w:val=\"NL\"/></w:rPr></w:pPr><w:r><w:rPr><w:lang w:val=\"NL\"/></w:rPr><w:t>Gewone tekst</w:t></w:r></w:p>"
       ++         "<w:p wsp:rsidR=\"001A6AA1\" wsp:rsidRPr=\"001A6AA1\" wsp:rsidRDefault=\"001A6AA1\" wsp:rsidP=\"001A6AA1\"><w:pPr><w:rPr><w:lang w:val=\"NL\"/></w:rPr></w:pPr></w:p>"
       ++         "<wx:sub-section>"
       ++            "<w:p wsp:rsidR=\"001A6AA1\" wsp:rsidRDefault=\"001A6AA1\" wsp:rsidP=\"001A6AA1\"><w:pPr><w:pStyle w:val=\"Heading2\"/><w:rPr><w:lang w:val=\"NL\"/></w:rPr></w:pPr><w:r><w:rPr><w:lang w:val=\"NL\"/></w:rPr><w:t>Hoofdstuk </w:t></w:r><w:r><w:rPr><w:lang w:val=\"NL\"/></w:rPr><w:t>lvl</w:t></w:r><w:r><w:rPr><w:lang w:val=\"NL\"/></w:rPr><w:t> 2</w:t></w:r></w:p>"
       ++            "<w:p wsp:rsidR=\"001A6AA1\" wsp:rsidRDefault=\"001A6AA1\" wsp:rsidP=\"001A6AA1\"><w:pPr><w:rPr><w:lang w:val=\"NL\"/></w:rPr></w:pPr><w:r><w:rPr><w:lang w:val=\"NL\"/></w:rPr><w:t>Gewone tekst</w:t></w:r></w:p>"
       ++            "<w:p wsp:rsidR=\"001A6AA1\" wsp:rsidRPr=\"001A6AA1\" wsp:rsidRDefault=\"001A6AA1\" wsp:rsidP=\"001A6AA1\"><w:pPr><w:rPr><w:lang w:val=\"NL\"/></w:rPr></w:pPr></w:p>"
       ++            "<wx:sub-section>"
       ++               "<w:p wsp:rsidR=\"001A6AA1\" wsp:rsidRDefault=\"001A6AA1\" wsp:rsidP=\"001A6AA1\"><w:pPr><w:pStyle w:val=\"Heading3\"/><w:rPr><w:lang w:val=\"NL\"/></w:rPr></w:pPr><w:r><w:rPr><w:lang w:val=\"NL\"/></w:rPr><w:t>Hoofdstuk </w:t></w:r><w:r><w:rPr><w:lang w:val=\"NL\"/></w:rPr><w:t>lvl</w:t></w:r><w:r><w:rPr><w:lang w:val=\"NL\"/></w:rPr><w:t> 3</w:t></w:r></w:p>"
       ++               "<w:p wsp:rsidR=\"001A6AA1\" wsp:rsidRDefault=\"001A6AA1\" wsp:rsidP=\"001A6AA1\"><w:pPr><w:rPr><w:lang w:val=\"NL\"/></w:rPr></w:pPr><w:r><w:rPr><w:lang w:val=\"NL\"/></w:rPr><w:t>Gewone tekst</w:t></w:r></w:p>"
       ++               "<w:p wsp:rsidR=\"001A6AA1\" wsp:rsidRPr=\"001A6AA1\" wsp:rsidRDefault=\"001A6AA1\" wsp:rsidP=\"001A6AA1\"><w:pPr><w:rPr><w:lang w:val=\"NL\"/></w:rPr></w:pPr></w:p>"
       ++               "<w:p wsp:rsidR=\"005F64C9\" wsp:rsidRPr=\"001A6AA1\" wsp:rsidRDefault=\"005F64C9\" wsp:rsidP=\"001A6AA1\"><w:pPr><w:rPr><w:lang w:val=\"NL\"/></w:rPr></w:pPr></w:p>"
       ++               "<w:sectPr wsp:rsidR=\"005F64C9\" wsp:rsidRPr=\"001A6AA1\">"
       ++                  "<w:pgSz w:w=\"12240\" w:h=\"15840\"/><w:pgMar w:top=\"1440\" w:right=\"1800\" w:bottom=\"1440\" w:left=\"1800\" w:header=\"708\" w:footer=\"708\" w:gutter=\"0\"/><w:cols w:space=\"708\"/><w:docGrid w:line-pitch=\"360\"/>"
       ++               "</w:sectPr>"
       ++            "</wx:sub-section>"
       ++         "</wx:sub-section>"
       ++     "</wx:sub-section>"
       ++""
           

