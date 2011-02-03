{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Misc (
     module DatabaseDesign.Ampersand.Misc.Languages
   , module DatabaseDesign.Ampersand.Misc.Options
   , module DatabaseDesign.Ampersand.Misc.Version

)where
   import DatabaseDesign.Ampersand.Misc.Languages (Lang(..),plural)
   import DatabaseDesign.Ampersand.Misc.Options   (getOptions,Options(..),defaultFlags,verboseLn,verbose,DocTheme(..),FspecFormat(..),PandocFormat(..) )
   import DatabaseDesign.Ampersand.Misc.Version   (ampersandCoreVersionBanner,versionNumber)



