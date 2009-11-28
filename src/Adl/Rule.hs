{-# OPTIONS_GHC -Wall #-}
module Adl.Rule    ( Rule(..),Rules
                   , RuleType(..)
                   , consequent, antecedent, cpu, ruleType, normExpr, multRules)     
where
   import Adl.FilePos                   ( FilePos(..),Numbered(..))
   import Adl.Concept                   ( Concept
                                        , Association(..)
                                        , MorphicId(..),Morphic(..))
   import Adl.MorphismAndDeclaration    ( Morphism(..),Declaration)
   import Adl.Expression                ( Expression(..),Expressions,v)
   import Adl.Prop
   import CommonClasses                 ( Identified(name,typ)
                                        , Explained(explain))
                                           
   type Rules = [Rule]
   data Rule =
  -- Ru c antc p cons cpu expla sgn nr pn
        Ru { rrsrt :: RuleType          -- ^ One of the following:
                                        --    | Implication if this is an implication;
                                        --    | Equivalence if this is an equivalence;
                                        --    | Truth  if this is an ALWAYS expression.
           , rrant :: Expression        -- ^ Antecedent
           , rrfps :: FilePos           -- ^ Position in the ADL file
           , rrcon :: Expression        -- ^ Consequent
           , r_cpu :: Expressions       -- ^ This is a list of subexpressions, which must be computed.
           , rrxpl :: String            -- ^ Explanation
           , rrtyp :: (Concept,Concept) -- ^ Sign of this rule
           , rrdcl :: Maybe (Prop,Declaration)  -- ^ The property, if this rule originates from a property on a Declaration
           , runum :: Int               -- ^ Rule number
           , r_pat :: String            -- ^ Name of pattern in which it was defined.
           }
  -- Sg p rule expla sgn nr pn signal
      | Sg { srfps :: FilePos           -- ^ position in the ADL file
           , srsig :: Rule              -- ^ the rule to be signalled
           , srxpl :: String            -- ^ explanation
           , srtyp :: (Concept,Concept) -- ^ type
           , runum :: Int               -- ^ rule number
           , r_pat :: String            -- ^ name of pattern in which it was defined.
           , srrel :: Declaration       -- ^ the signal relation
           }
  -- Gc p antc cons cpu _ _ _
      | Gc { grfps :: FilePos           -- ^ position in the ADL file
           , grspe :: Morphism          -- ^ specific
           , grgen :: Expression        -- ^ generic
           , r_cpu :: Expressions       -- ^ This is a list of subexpressions, which must be computed.
           , grtyp :: (Concept,Concept) -- ^ declaration
           , runum :: Int               -- ^ rule number
           , r_pat :: String            -- ^ name of pattern in which it was defined.
           }
  -- Fr t d expr pn  -- represents an automatic computation, such as * or +.
      | Fr { --fraut :: AutType           -- ^ the type of automatic computation
             frdec :: Declaration       -- ^ where the result is to be stored
           , frcmp :: Expression        -- ^ expression to be computed
           , r_pat :: String            -- ^ name of pattern in which it was defined.
           } deriving (Eq)
   data RuleType = Implication | Equivalence | Truth | Generalization | Automatic deriving (Eq,Show)

   -- | WAAROM? Dit mag hier wel even expliciet worden uitgelegd. Hier zit vast een heel verhaal achter... Stef?
--   data AutType = Clos0 | Clos1 deriving (Eq,Show)
   instance Show Rule where
    showsPrec _ x@(Ru{})  | rrsrt x==Implication = showString$ show(rrant x) ++ " |- " ++ (show$rrcon x)
                          | rrsrt x==Equivalence = showString$ show(rrant x) ++ " = " ++ (show$rrcon x)
                          | rrsrt x==Truth = showString$ show(rrcon x)
                          | otherwise = showString$ show$grgen x
    showsPrec _ x@(Sg{})  = showString$ "SIGNAL: " ++ (show$srsig x)
    showsPrec _ x@(Gc{})  = showString$ "Gc " ++ (show$grgen x)
    showsPrec _ x@(Fr{})  = showString$ "Fr " ++ (show$grgen x)
        
   instance Numbered Rule where
    pos r = case r of
              Ru{}  ->  rrfps r
              Sg{}  ->  srfps r
              Gc{}  ->  grfps r
              Fr{}  ->  Nowhere
    nr r = case r of
              Fr{}  ->  0
              _     ->  runum r

      
   instance Identified Rule where
    name r = "Rule"++show (runum r)
    typ _ = "Rule_"
    
   -- | Han, wat hieronder gebeurt vind ik raar: twee varianten waar hetzelfde uitkomt (in source en target). WAAROM? Welke bedoeling heb je daarmee? Geen? TODO: vereenvoudigen.
   instance Association Rule where
    source r  = fst (sign r)
    target r  = snd (sign r)
    sign r@Ru{} = rrtyp r
    sign r@Sg{} = srtyp r
    sign r@Gc{} = grtyp r
    sign _      = error("!Fatal (module Rule 93): undefined sign")

   instance Explained Rule where
    explain _ r = case r of         -- TODO: to allow explainations in multiple languages, change to:  explain options d@Sgn{} = etc...
                   Ru{}  ->  rrxpl r
                   Sg{}  ->  srxpl r
                   _     ->  ""

   instance MorphicId Rule where
    isIdent r = isIdent (normExpr r)

   instance Morphic Rule where
    multiplicities _  = []
    isMph r = case r of
                Ru{rrsrt=Truth} -> isMph (rrcon r)
                Sg{}            -> isMph (srsig r)
                _               -> False
                
--    isMph r  | ruleType r==Truth = isMph (consequent r)
--             | otherwise       = False
    flp r = case r of
                Ru{} -> r{rrant = if rrsrt r == Truth
                                  then error ("!Fatal (module Rule 118): illegal call to antecedent in flp ("++show r++")")
                                  else flp (rrant r)
                         ,rrcon = flp (rrcon r)
                         ,rrtyp = (target (rrtyp r),source (rrtyp r))
                         }
                _    -> error ("!Fatal (module Rule 118): flp undefined for rule:\n "++show r)
  --  isIdent r = error ("!Fatal (module Rule 126): isIdent not applicable to any rule:\n "++showHS "" r)
    typeUniq r | ruleType r==Truth = typeUniq (antecedent r)
               | otherwise       = typeUniq (antecedent r) && typeUniq (consequent r)
--    isIdent r = isIdent (normExpr r)
    isProp r = isProp (normExpr r)
    isTrue r | ruleType r==Truth  = isTrue (consequent r)
             | otherwise        = isTrue (consequent r) || isFalse (consequent r)
    isFalse r| ruleType r==Truth  = isFalse (consequent r)
             | otherwise        = isFalse (consequent r) && isTrue (consequent r)
    isSignal r = case r of
                   Sg{} -> True
                   _    -> False 
    isNot r  | ruleType r==Truth  = isNot (consequent r)
             | otherwise        = False  -- TODO: check correctness!

   normExpr :: Rule -> Expression
   normExpr rule
    | isSignal rule      = v (sign rule)
    | ruleType rule==Truth = consequent rule
    | ruleType rule==Implication = Fu [Cp (antecedent rule), consequent rule]
    | ruleType rule==Equivalence = Fi [ Fu [antecedent rule, Cp (consequent rule)]
                              , Fu [Cp (antecedent rule), consequent rule]]
    | otherwise          = error("!Fatal (module Rule 148): Cannot make an expression of "++show rule)



   ruleType :: Rule -> RuleType
   ruleType r = case r of 
                   Ru{} -> rrsrt r
                   Sg{} -> ruleType (srsig r)
                   Gc{} -> Generalization
                   Fr{} -> Automatic

   antecedent :: Rule -> Expression
   antecedent r = case r of
                   Ru{rrsrt = Truth} -> error ("!Fatal (module Rule 161): illegal call to antecedent of rule "++show r)
                   Ru{} -> rrant r
                   Sg{} -> antecedent (srsig r)
                   Gc{} -> Tm (grspe r)
                   Fr{} -> frcmp r
                   
   consequent :: Rule -> Expression
   consequent r = case r of
                   Ru{} -> rrcon r
                   Sg{} -> consequent (srsig r)
                   Gc{} -> grgen r
                   Fr{} -> error ("!Fatal (module Rule 172):  Tm (makeMph (frdec r)) might generate a loop.")

   cpu :: Rule -> Expressions
   cpu r = case r of
                   Ru{} -> r_cpu r
                   Sg{} -> [] -- TODO nakijken: Moet dit niet de signaalrelatie zijn?
                   Gc{} -> r_cpu r
                   Fr{} -> error ("!Fatal (module Rule 179): [Tm (makeMph (frdec r))] might generate a loop.")

   multRules :: Declaration -> [Rule]
   multRules d
     = [h p| p<-multiplicities d, p `elem` [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
           , if source d==target d || p `elem` [Uni,Tot,Inj,Sur] then True else
              error ("!Fatal (module CC_aux): Property "++show p++" requires equal source and target domains (you specified "++name (source d)++" and "++name (target d)++").") ]
      where h Sym = Ru Equivalence (F [Tm r]) (pos d) (F [Tm r'])        [] (name d++"["++name (source d)++"*"++name (source d)++"] is symmetric.")     sgn Nothing (nr d) ""
            h Asy = Ru Implication (Fi [F [Tm r], F [Tm r']]) (pos d) id' [] (name d++"["++name (source d)++"*"++name (source d)++"] is antisymmetric.") sgn Nothing (nr d) ""
            h Trn = Ru Implication (F [Tm r, Tm r]) (pos d) (F [Tm r])   [] (name d++"["++name (source d)++"*"++name (source d)++"] is transitive.")    sgn Nothing (nr d) ""
            h Rfx = Ru Implication id' (pos d) (F [Tm r])                 [] (name d++"["++name (source d)++"*"++name (source d)++"] is reflexive.")     sgn Nothing (nr d) ""
            h Uni = Ru Implication (F [Tm r',Tm r]) (pos d) id''          [] (name d++"["++name (source d)++"*"++name (target d)++"] is univalent")      sgn Nothing (nr d) ""
            h Sur = Ru Implication id'' (pos d) (F [Tm r',Tm r])          [] (name d++"["++name (source d)++"*"++name (target d)++"] is surjective")     sgn Nothing (nr d) ""
            h Inj = Ru Implication (F [Tm r,Tm r']) (pos d) id'           [] (name d++"["++name (source d)++"*"++name (target d)++"] is injective")      sgn Nothing (nr d) ""
            h Tot = Ru Implication id' (pos d) (F [Tm r,Tm r'])           [] (name d++"["++name (source d)++"*"++name (target d)++"] is total")          sgn Nothing (nr d) ""
            h Aut = error("!Fatal (module Language): multRules not defined for property 'Aut'")
            sgn   = (source d,source d)
            r     = Mph (name d)                (pos d) [] (source d,target d) True d
            r'    = flp (r ) 
 --           r'' t = Mph (t++"["++(name d)++"]") (pos d) [] (source d,target d) True d
            id'    = F [Tm (I [source d] (source d) (source d) True)]
            id''    = F [Tm (I [target d] (target d) (target d) True)]
 
