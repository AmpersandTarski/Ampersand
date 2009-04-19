{-# OPTIONS_GHC -Wall #-}
module Adl.Rule    ( Rule(..),Rules
                   , RuleType(..)
                   , consequent, antecedent, cpu, ruleType, normExpr)     
where
   import Adl.FilePos                   ( FilePos(..),Numbered(..))
   import Adl.Concept                   ( Concept
                                        , Association(..)
                                        , MorphicId(..),Morphic(..))
   import Adl.MorphismAndDeclaration    ( Morphism,Declaration)
   import Adl.Expression                ( Expression(..),Expressions,v)
   import CommonClasses                 ( Identified(name,typ)
                                        , ABoolAlg(lub,order)
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
           } deriving (Eq,Show)
   data RuleType = Implication | Equivalence | Truth | Generalization | Automatic deriving (Eq,Show)

   -- | WAAROM? Dit mag hier wel even expliciet worden uitgelegd. Hier zit vast een heel verhaal achter... Stef?
--   data AutType = Clos0 | Clos1 deriving (Eq,Show)
        
   instance Numbered Rule where
    pos r = case r of
              Ru{}  ->  rrfps r
              Sg{}  ->  srfps r
              Gc{}  ->  grfps r
              Fr{}  ->  Nowhere
    nr r = case r of
              Ru{}  ->  runum r
              Sg{}  ->  runum r
              Gc{}  ->  runum r
              Fr{}  ->  0

      
   instance Identified Rule where
    name r = "Rule"++show (runum r)
    typ _ = "Rule_"
    
   -- | Han, wat hieronder gebeurt vind ik raar: twee varianten waar hetzelfde uitkomt (in source en target). WAAROM? Welke bedoeling heb je daarmee? Geen? TODO: vereenvoudigen.
   instance Association Rule where
    source r  = fst (sign r)
    target r  = snd (sign r)
    sign r   | ruleType r==Truth = sign (consequent r)
             | otherwise         = if sign (antecedent r) `order` sign (consequent r) then sign (antecedent r) `lub` sign (consequent r) else
                                            error ("(module Rule) Fatal: incompatible signs in "++show r)

   instance Explained Rule where
    explain r = case r of
                   Ru{}  ->  rrxpl r
                   Sg{}  ->  srxpl r
                   Gc{}  ->  ""
                   Fr{}  ->  ""

   instance MorphicId Rule where
    isIdent r = isIdent (normExpr r)

   instance Morphic Rule where
    multiplicities _  = []
    isMph r = case r of
                Ru{rrsrt=Truth} -> isMph (rrcon r)
                Ru{}            -> False
                Sg{}            -> isMph (srsig r)
                Gc{}            -> False
                Fr{}            -> False
                
--    isMph r  | ruleType r==Truth = isMph (consequent r)
--             | otherwise       = False
    flp r = case r of
                Ru{} -> r{rrant = if rrsrt r == Truth
                                  then error ("(Module Classes.Morphic:) illegal call to antecedent in flp ("++show r++")")
                                  else flp (rrant r)
                         ,rrcon = flp (rrcon r)
                         ,rrtyp = (target (rrtyp r),source (rrtyp r))
                         }
                Sg{}            -> undefined
                Gc{}            -> undefined
                Fr{}            -> undefined
  --  isIdent r = error ("(module CC_aux: isIdent) not applicable to any rule:\n "++showHS "" r)
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
    | otherwise          = error("Fatal (module CC_aux): Cannot make an expression of "++show rule)



   ruleType :: Rule -> RuleType
   ruleType r = case r of 
                   Ru{} -> rrsrt r
                   Sg{} -> ruleType (srsig r)
                   Gc{} -> Generalization
                   Fr{} -> Automatic

   antecedent :: Rule -> Expression
   antecedent r = case r of
                   Ru{rrsrt = Truth} -> error ("(Module Adl.Rule:) illegal call to antecedent of rule "++show r)
                   Ru{} -> rrant r
                   Sg{} -> antecedent (srsig r)
                   Gc{} -> Tm (grspe r)
                   Fr{} -> frcmp r
                   
   consequent :: Rule -> Expression
   consequent r = case r of
                   Ru{} -> rrcon r
                   Sg{} -> consequent (srsig r)
                   Gc{} -> grgen r
                   Fr{} -> error (" Tm (makeMph (frdec r)) genereert wellicht een loop.")

   cpu :: Rule -> Expressions
   cpu r = case r of
                   Ru{} -> r_cpu r
                   Sg{} -> [] -- TODO nakijken: Moet dit niet de signaalrelatie zijn?
                   Gc{} -> r_cpu r
                   Fr{} -> error (" [Tm (makeMph (frdec r))] genereert wellicht een loop.")

--   uncomp :: Rule -> Rule
--   uncomp r = case r of
--                   Ru{} -> r{r_cpu = []}
--                   Sg{} -> r
--                   Gc{} -> r{r_cpu = []}
--                   Fr{} -> r
--
                       