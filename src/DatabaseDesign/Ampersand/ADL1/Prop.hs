{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.Prop (Prop(..),Props
                ,flipProps,allprops,endoprops)
where
   import DatabaseDesign.Ampersand.Core.ParseTree      (Props,Prop(..))
   allprops,endoprops::Props
   allprops = [Uni,Inj,Sur,Tot,Sym,Asy,Trn,Rfx,Irf]
   endoprops = [Sym,Asy,Trn,Rfx,Irf]

   
   flipProps :: Props -> Props
   flipProps = map flipProp 

   flipProp :: Prop -> Prop
   flipProp Uni = Inj
   flipProp Tot = Sur
   flipProp Sur = Tot
   flipProp Inj = Uni
   flipProp x = x
