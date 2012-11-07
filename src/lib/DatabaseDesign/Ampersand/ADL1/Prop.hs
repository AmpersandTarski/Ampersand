{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.Prop (Prop(..),Props
                ,allprops,endoprops)
where
   import DatabaseDesign.Ampersand.Core.ParseTree      (Props,Prop(..))
   allprops,endoprops::Props
   allprops = [Uni,Inj,Sur,Tot,Sym,Asy,Trn,Rfx,Irf]
   endoprops = [Sym,Asy,Trn,Rfx,Irf]

   
