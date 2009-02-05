
module Adl.Rule where
   import Adl.FilePos
   import Adl.Concept
   import Adl.MorphismAndDeclaration
   import Adl.Expression
   import CommonClasses(Identified(name,typ)
                        , ABoolAlg(glb,lub,order)
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
      | Fr { fraut :: AutType           -- ^ the type of automatic computation
           , frdec :: Declaration       -- ^ where the result is to be stored
           , frcmp :: Expression        -- ^ expression to be computed
           , r_pat :: String            -- ^ name of pattern in which it was defined.
           } deriving (Eq,Show)
   data RuleType = Implication | Equivalence | Truth | Generalization | Automatic deriving (Eq,Show)

   -- | WAAROM? Dit mag hier wel even expliciet worden uitgelegd. Hier zit vast een heel verhaal achter... Stef?
   data AutType = Clos0 | Clos1 deriving (Eq,Show)
        
   instance Numbered Rule where
    pos (Ru _ _ p _ _ _ _ _ _) = p
    pos (Sg p _ _ _ _ _ _)     = p
    pos (Gc p _ _ _ _ _ _)     = p
    pos r = posNone
    nr  (Ru _ _ _ _ _ _ _ n _) = n
    nr  (Sg _ _ _ _ n _ _)     = n
    nr  (Gc _ _ _ _ _ n _)     = n
    nr  r = 0


--   instance Eq Rule where        -- WAAROM :TODO Stef, deze Eq mistte zijn where clause. Wil jij dit valideren? 
--     r == r' = case ( r, r') of
--       (Ru{} , Ru{})  -> r_pat r == r_pat r' &&
--                         runum r == runum r'  
--       (Sg{} , Sg{})  -> r_pat r == r_pat r' &&
--                         runum r == runum r'
--       (Gc{} , Gc{})  -> r_pat r == r_pat r' &&
--                         runum r == runum r'
--       (Fr{} , Fr{})  -> r_pat r == r_pat r' &&
--                         frcmp r == frcmp r' &&
--                         frdec r == frdec r' &&
--                         fraut r == fraut r'
--       ( _   , _   )  -> False
--   instance Show Rule 
      
   instance Identified Rule where
    name r = "Rule"++show (runum r)
    typ r = "Rule_"
   -- | Han, wat hieronder gebeurt vind ik raar: twee varianten waar hetzelfde uitkomt (in source en target). WAAROM? Welke bedoeling heb je daarmee? Geen? TODO: vereenvoudigen.
   instance Association Rule where
    source r | ruleType r==Truth = fst (sign r)
             | otherwise              = fst (sign r)
    target r | ruleType r==Truth = snd (sign r)
             | otherwise              = snd (sign r)
    sign r   | ruleType r==Truth = sign (consequent r)
             | otherwise              = if sign (antecedent r) `order` sign (consequent r) then sign (antecedent r) `lub` sign (consequent r) else
                                            error ("(module Rule) Fatal: incompatible signs in "++misbruiktShowHS "" r)

   instance Explained Rule where
    explain (Ru _ _ _ _ _ expla _ _ _) = expla
    explain (Sg _ _ expla _ _ _ _)     = expla
    explain r                          = ""

   ruleType :: Rule -> RuleType
   ruleType r = case r of 
                   Ru{} -> rrsrt r
                   Sg{} -> ruleType (srsig r)
                   Gc{} -> Generalization
                   Fr{} -> Automatic

   antecedent :: Rule -> Expression
   antecedent r = case r of
                   Ru{rrsrt = Truth} -> error ("(Module ADLdef:) illegal call to antecedent of rule "++show r)
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
                   Fr{} -> error (" [Tm (makeMph (frdec r))] genereerdt wellicht een loop.")

   uncomp :: Rule -> Rule
   uncomp r = case r of
                   Ru{} -> r{r_cpu = []}
                   Sg{} -> r
                   Gc{} -> r{r_cpu = []}
                   Fr{} -> r

   signalen :: Rules -> Rules      -- Deze functie is toegevoegd vanwege gebruik in 'Morphical'.  
   signalen  rls = [r| r<-rls, isSignaal r]

   isSignaal :: Rule -> Bool
   isSignaal r = case r of
                  Sg{} -> True
                  _    -> False            