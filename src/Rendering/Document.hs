{-# OPTIONS_GHC -Wall #-}
module Rendering.Document where
   import Text.Html
   import Options
   import Languages
   -----------------some new data types for simple Document structures--------

   data DTxt = Text { txt :: String
                    } 
             | RefSection { txt :: String
                    }
--   data DCell = Cell [DTxt]
--   data DRow  = Row [DCell]
--   data DTable = Table [DRow]

   data DLItem = Item { liDTxt :: DTxt}
   data DSContent = List { items:: [DLItem]}
                  | Par { dtxts :: [DTxt] }
                  | Section {dsLevel :: Int           -- level of header (lijkt nodig voor LaTeX) 
                            ,dshead  :: DTxt          -- header of the section
                            ,dscnts  :: [DSContent]   -- stuff in the section
                            }
                  
   data Document = Doc { dflgs :: Options
                       , dtitle :: DTxt
                       , dcont :: [DSContent]
                       }

   class ToHTML a where
--     render2LaTeX :: a -> String
     render2Html :: a -> Html
--   render2Rtf  :: a -> RTF  -- Komt ooit nog wel eens...

          
   instance ToHTML DSContent where
      render2Html x =
           case x of
             List{} -> dlist (foldr (+++) noHtml (map render2Html (items x)))
             Par {} -> paragraph (foldr (+++) noHtml (map render2Html (dtxts x)))       
             Section{} -> header (render2Html (dshead x))
                      +++ foldr (+++) noHtml (map render2Html (dscnts x))

   instance ToHTML DLItem where
       render2Html item = dterm (render2Html (liDTxt item))
        
   instance ToHTML DTxt where
     render2Html dtxt  = case dtxt of 
                           Text str -> stringToHtml str
                       

   docTitle :: Options -> String
   docTitle flags = ( case language flags of
                        Dutch -> "Functionele Specificatie van "
                        English -> "Functional Specification of "
                    )
                  ++ baseName flags      
--------------------- Spullen hieronder moeten nog eens op de schop. (HJO, 17 feb 2009)

 