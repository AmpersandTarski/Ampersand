module Prototype.Garbage where

import Char  (isAlpha,isAlphaNum,ord,isUpper,toLower,toUpper)  --TODO -> imported for function enc
import Adl


--TODO -> Prototype.RelBinGenBasics -> copied from Auxiliaries because disabled (why disabled?)
enc :: Bool -> String -> String
enc upper (c:cs) | not (isAlphaNum c) = '_': htmlEnc c ++ enc upper cs
                 | isUpper c==upper   = c: enc upper cs
                 | otherwise          = '_': c: enc (not upper) cs
  where 
     htmlEnc = reverse . take 3 . (++"00") . reverse . show . ord
enc _ "" = ""

--TODO -> Prototype.RelBinGenBasics -> copied from CC_aux because disabled (why disabled?)
oneMorphism :: Expression -> Bool
oneMorphism (Tm _)    = True
oneMorphism (Tc f)    = oneMorphism f
oneMorphism (F [t])   = oneMorphism t
oneMorphism (F ts)    = False
oneMorphism (Fd [t])  = oneMorphism t
oneMorphism (Fd ts)   = False
oneMorphism (Fu [f])  = oneMorphism f
oneMorphism (Fu fs)   = False
oneMorphism (Fi [f])  = oneMorphism f
oneMorphism (Fi fs)   = False
oneMorphism (K0 e)    = oneMorphism e
oneMorphism (K1 e)    = oneMorphism e
oneMorphism (Cp e)    = oneMorphism e

--TODO -> Prototype.RelBinGenBasics -> isProperty in CC_aux has type Morphism -> Bool
isProperty :: Expression -> Bool
isProperty _ = False


--TODO -> Prototype.ObjBinGenConnectToDataBase -> copied from Auxiliaries because disabled (why disabled?)
commaNL :: String -> [String] -> String
commaNL str [a,b,c]= a++", "++b++" "++str++" "++c
commaNL str [a,b]  = a++" "++str++" "++b
commaNL _ [a]    = a
commaNL str (a:as) = a++", "++commaNL str as
commaNL _ []     = ""
