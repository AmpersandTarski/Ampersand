{-# LANGUAGE RankNTypes, ExistentialQuantification, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing 
       (parseIO,parse,getMsgs,evalSteps
       , Steps(..),Pair(..)
                      ,Symbol(..),pPacked
                      ,Parser
                      ,pList,pListSep,pList1,pList1Sep,pSym
                      ,opt
                      ,(<??>), (<**>), Sequence((<$>), (<$), (<*>), (<*), (*>), pSucceed )
                      ,Alternative(..)
                      ,IsParser
                      ,Message(..))
--   (
--   Result,
--   mapOnePars,
--   libSucceed,
--   InputState(..),
--   OutputState(..),
--   Sequence(..),
--   Alternative(..),
--   Symbol(..),
--   SymParser(..),
--   SplitParser(..),
--   IsParser,
--   RealParser(..),
--   RealRecogn(..),
--   Either'(..),
--   ParsRec(..),
--   Message(..),
--
--   AnaParser,
--   Parser,
--   Steps(..),
--   Pair(..),
--   Exp(..),
--   pLocate,
--   pToks,
--   list_of ,
--   usealg ,
--   pMerged,
--   pLength,
--   (<||>) ,
--   pAnySym,
--   pAny ,
--   pChainl,
--   pChainl_ng,
--   pChainl_gr,
--   pChainr,
--   pChainr_ng,
--   pChainr_gr,
--   pList1Sep,
--   pList1Sep_ng,
--   pList1Sep_gr,
--   pListSep,
--   pListSep_ng,
--   pListSep_gr,
--   pList1,
--   pList1_ng,
--   pList1_gr,
--   pList,
--   pList_ng,
--   pList_gr,
--   list_alg,
--   pFoldr1Sep,
--   pFoldr1Sep_ng,
--   pFoldr1Sep_gr,
--   pFoldrSep,
--   pFoldrSep_ng,
--   pFoldrSep_gr,
--   pFoldr1,
--   pFoldr1_ng,
--   pFoldr1_gr,
--   pFoldr,
--   pFoldr_gr,
--   pFoldr_ng,
--   pPacked,
--   (<?>),
--   (<??>),
--   (<$$>),
--   (<**>),
--  -- (*>),
--  -- (<*),
--  -- (<$),
--   (<+>),
--   asOpt,
--   asList1,
--   asList,
--   opt ,
--   pExcept,
--   (<..>) ,
--   mnz,
--   acceptsepsilon,
--   p2p,pPermsSep,pPerms,add,(~$~),(~*~),
--   systemerror,
--   usererror,
--   handleEof,
--   pDynL,
--   pDynE,
--   -- getErrors,
--  -- getMsgs,
--   parse ,parsebasic,parseIO,
--   evalSteps,evalStepsIO,getMsgs,
--  -- pCostRange,
--  -- pCostSym,
--  -- pSym,
--  -- pRange,
--  -- getfirsts,
--  -- setfirsts,
--  -- (<*>),
--  -- pSucceed,
--  -- pLow,
--  -- (<$>),
--  -- (<|>),
--  -- pFail,
--   pMap,
--   pWrap,
--   val
--   )
   where
   import Data.Maybe
   --import PrelGHC
   --import IOExts
   import System.IO.Unsafe
   import DatabaseDesign.Ampersand.Basics  
   import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Input.ADL1.UU_Parsing"

   btLookup :: BinSearchTree (a -> Ordering) (Maybe b) -> a -> Maybe b
   tab2tree :: Ord a => [(SymbolR a,b)] -> BinSearchTree (a -> Ordering) b
   pLocate :: (Alternative a, SymParser a b, Sequence a) => [[b]] -> a [b]
   pToks :: (Sequence a, SymParser a b) => [b] -> a [b]
   list_of :: Sequence a => a b -> ([c],a ([b] -> [b]),d -> d)
   usealg :: Sequence a => (b -> c,d) -> a b -> (d,a c,e -> e)
   pMerged :: (Symbol b, Sequence a, Alternative a, Show (Exp b), SymParser a b, SplitParser a) => c -> (d,a (d -> d),c -> d -> e) -> a e
   (<||>) :: (Sequence a, Alternative a) => (b,a (c -> c),d -> e -> f) -> (g,a (h -> h),f -> i -> j) -> ((b,g),a ((c,h) -> (c,h)),d -> (e,i) -> j)
   pAnySym :: (Alternative a, SymParser a b) => [b] -> a b
   pAny :: Alternative a => (b -> a c) -> [b] -> a c
   pChainl :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => a (c -> c -> c) -> a c -> a c
   pChainl_ng :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => a (c -> c -> c) -> a c -> a c
   pChainl_gr :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => a (c -> c -> c) -> a c -> a c
   pChainr :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => a (c -> c -> c) -> a c -> a c
   pChainr_ng :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Alternative a, Sequence a) => a (c -> c -> c) -> a c -> a c
   pChainr_gr :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => a (c -> c -> c) -> a c -> a c
   pList1Sep :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => a c -> a d -> a [d]
   pList1Sep_ng :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => a c -> a d -> a [d]
   pList1Sep_gr :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => a c -> a d -> a [d]
   pListSep :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => a c -> a d -> a [d]
   pListSep_ng :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Alternative a, Sequence a) => a c -> a d -> a [d]
   pListSep_gr :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => a c -> a d -> a [d]
   pList1 :: (Symbol b, SymParser a b, Sequence a, Show (Exp b), SplitParser a, Alternative a) => a c -> a [c]
   pList1_ng :: (Symbol b, SymParser a b, Sequence a, Show (Exp b), SplitParser a, Alternative a) => a c -> a [c]
   pList1_gr :: (Symbol b, SymParser a b, Sequence a, Show (Exp b), SplitParser a, Alternative a) => a c -> a [c]
   pList :: (Symbol b, SymParser a b, Show (Exp b), SplitParser a, Sequence a, Alternative a) => a c -> a [c]
   pList_ng :: (Symbol b, SymParser a b, Show (Exp b), SplitParser a, Sequence a, Alternative a) => a c -> a [c]
   pList_gr :: (Symbol b, SymParser a b, Show (Exp b), SplitParser a, Sequence a, Alternative a) => a c -> a [c]
   list_alg :: (a -> [a] -> [a],[b])
   pFoldr1Sep :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => (c -> d -> d,d) -> a e -> a c -> a d
   pFoldr1Sep_ng :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => (c -> d -> d,d) -> a e -> a c -> a d
   pFoldr1Sep_gr :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => (c -> d -> d,d) -> a e -> a c -> a d
   pFoldrSep :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => (c -> d -> d,d) -> a e -> a c -> a d
   pFoldrSep_ng :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Alternative a, Sequence a) => (c -> d -> d,d) -> a e -> a c -> a d
   pFoldrSep_gr :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => (c -> d -> d,d) -> a e -> a c -> a d
   pFoldr1 :: (Symbol b, SymParser a b, Sequence a, Show (Exp b), SplitParser a, Alternative a) => (c -> d -> d,d) -> a c -> a d
   pFoldr1_ng :: (Symbol b, SymParser a b, Sequence a, Show (Exp b), SplitParser a, Alternative a) => (c -> d -> d,d) -> a c -> a d
   pFoldr1_gr :: (Symbol b, SymParser a b, Sequence a, Show (Exp b), SplitParser a, Alternative a) => (c -> d -> d,d) -> a c -> a d
   pFoldr :: (Symbol b, SymParser a b, Show (Exp b), SplitParser a, Sequence a, Alternative a) => (c -> d -> d,d) -> a c -> a d
   pFoldr_gr :: (Symbol b, SymParser a b, Show (Exp b), SplitParser a, Sequence a, Alternative a) => (c -> d -> d,d) -> a c -> a d
   pFoldr_ng :: (Symbol b, SymParser a b, Show (Exp b), SplitParser a, Sequence a, Alternative a) => (c -> d -> d,d) -> a c -> a d
   pPacked :: Sequence a => a b -> a c -> a d -> a d
   (<?>) :: SymParser a b => a c -> String -> a c
   (<??>) :: (Symbol b, Sequence a, Alternative a, Show (Exp b), SymParser a b, SplitParser a) => a c -> a (c -> c) -> a c
   (<$$>) :: Sequence a => (b -> c -> d) -> a c -> a (b -> d)
   (<**>) :: Sequence a => a b -> a (b -> c) -> a c
   (<+>) :: Sequence a => a b -> a c -> a (b,c)
   asOpt :: SymParser a b => Exp b -> a c -> a c
   asList1 :: SymParser a b => Exp b -> a c -> a c
   asList :: SymParser a b => Exp b -> a c -> a c
   opt :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b), Sequence a, Alternative a) => a c -> c -> a c
   pExcept :: (Alternative a, SymParser a b, Eq (SymbolR b), Symbol b) => (b,b,b) -> [b] -> a b
   (<..>) :: SymParser a b => b -> b -> a b
   mnz :: (Symbol b, SplitParser a, SymParser a b, Show (Exp b)) => a c -> d -> d
   acceptsepsilon :: SplitParser a => a b -> Bool
   p2p :: (Alternative a, Sequence a) => a b -> a c -> Perms a d -> a d
   pPermsSep :: (Alternative a, Sequence a) => a b -> Perms a c -> a c
   pPerms :: (Alternative a, Sequence a) => Perms a b -> a b
   add :: Sequence a => Perms a (b -> c) -> (Maybe (a b),Maybe (a b)) -> Perms a c
   (~$~) :: (Sequence a, SplitParser a) => (b -> c) -> a b -> Perms a c
   (~*~) :: (Sequence a, SplitParser a) => Perms a (b -> c) -> a b -> Perms a c
   systemerror :: String -> String -> a
   usererror :: String -> a
   except :: Symbol a => SymbolR a -> [a] -> [SymbolR a]
   symRS :: Ord a => SymbolR a -> a -> Ordering
   symInRange :: Ord a => SymbolR a -> a -> Bool
   mk_range :: Ord a => a -> a -> SymbolR a
   mergeTables :: (Symbol a, Ord b) => [(SymbolR a,ParsRec c d b e)] -> [(SymbolR a,ParsRec c d b e)] -> [(SymbolR a,ParsRec c d b e)]
   nat_add :: Nat -> Nat -> Nat
   nat_min :: Nat -> Nat -> (Nat,(a,a) -> (a,a))
   nat_le :: Nat -> Nat -> Bool
   lib_correct :: Ord a => (b -> c -> Steps d a) -> (b -> c -> Steps d a) -> b -> c -> Steps d a
   mkParser :: InputState a b => Maybe (Bool,Either c (ParsRec a d b c)) -> OneDescr a d b c -> AnaParser a d b c
   mapOnePars :: (ParsRec a b c d -> ParsRec e f c g) -> (Nat -> Nat) -> OneDescr a b c d -> OneDescr e f c g
   anaSetFirsts :: InputState a b => Exp b -> AnaParser a c b d -> AnaParser a c b d
   anaGetFirsts :: AnaParser a b c d -> Exp c
   pLength :: AnaParser a b c d -> Nat
   anaCostSym :: SymParser a b => Int{-I-} -> b -> b -> a b
   anaCostRange :: InputState a b => Int{-I-} -> b -> SymbolR b -> AnaParser a c b b
   orOneOneDescr :: (Ord a, Ord (Exp a), Eq (SymbolR a)) => OneDescr b c a d -> OneDescr b c a d -> Bool -> OneDescr b c a d
   seqZeroZero :: Maybe (Bool,Either a b) -> Maybe (Bool,Either c (ParsRec d e f c)) -> (a -> ParsRec d e f c -> g) -> (b -> ParsRec d e f c -> g) -> (a -> c -> h) -> Maybe (Bool,Either h g)
   anaSeq :: (Ord (Exp a), Eq (SymbolR a), InputState b a) => (c -> ParsRec d e a f -> ParsRec b g a h) -> (ParsRec i j a c -> ParsRec d e a f -> ParsRec b g a h) -> (c -> f -> h) -> AnaParser i j a c -> AnaParser d e a f -> AnaParser b g a h
   anaOr :: (InputState a b, Ord (Exp b), Eq (SymbolR b), Show (Exp b)) => AnaParser a c b d -> AnaParser a c b d -> AnaParser a c b d
   anaDynN :: InputState a b => Exp b -> Nat -> SymbolR b -> TableEntry a c b d -> AnaParser a c b d
   anaDynL :: ParsRec a b c d -> AnaParser a b c d
   anaDynE :: ParsRec a b c d -> AnaParser a b c d
   anaLow :: a -> AnaParser b c d a
   anaSucceed :: a -> AnaParser b c d a
   pEmpty :: ParsRec a b c d -> (Bool,Either d (ParsRec a b c d)) -> AnaParser a b c d
   noOneParser :: OneDescr a b c d
   anaFail :: AnaParser a b c d
   traverse :: Pairs -> Pairs -> Steps a b -> Int{-I-} -> Pairs
   libCorrect :: Ord a => Steps b a -> Steps c a -> (b -> d) -> (c -> d) -> Steps d a
   libBest' :: Ord a => Steps b a -> Steps c a -> (b -> d) -> (c -> d) -> Steps d a
   libBest :: Ord a => Steps b a -> Steps b a -> Steps b a
   eor :: Ord (Exp a) => Exp a -> Exp a -> Exp a
   addexpecting :: Ord a => Exp a -> Steps b a -> Steps b a
   marks :: String
   addToMessage :: Ord (Exp a) => Message a -> Exp a -> Message a
   getStart :: Message a -> Exp a
   getMsgs :: Steps a b -> [Message b]
   evalStepsIO :: Symbol a => Steps b a -> IO b
   evalSteps :: Steps a b -> a
   hasSuccess :: Steps a b -> Bool
   starting :: Steps a b -> Exp b
   libFail :: ParsRec a b c d
   libOr :: Ord a => ParsRec b c a d -> ParsRec b c a d -> ParsRec b c a d
   libSeqR :: ParsRec a b c d -> ParsRec a e c f -> ParsRec a e c f
   libSeqL :: ParsRec a b c d -> ParsRec a e c f -> ParsRec a b c d
   libDollarR :: a -> ParsRec b c d e -> ParsRec b c d e
   libDollarL :: a -> ParsRec b c d e -> ParsRec b f d a
   libDollar :: OutputState a => (b -> c) -> ParsRec d a e b -> ParsRec d a e c
   libSeq :: OutputState a => ParsRec b a c (d -> e) -> ParsRec b f c d -> ParsRec b f c e
   libSucceed :: a -> ParsRec b c d a
   libInsert :: InputState a b => Int{-I-} -> b -> Exp b -> ParsRec a c b b
   libAccept :: InputState a b => ParsRec a c b b
   unR :: RealRecogn a b -> (a b -> Result c b) -> a b -> Result c b
   unP :: RealParser a b c d -> (d -> b e f -> g) -> (a c -> Result (b e f) c) -> a c -> Result g c
   pDynN :: InputState a b => Exp b -> Nat -> SymbolR b -> TableEntry a c b d -> AnaParser a c b d
   pDynL :: ParsRec a b c d -> AnaParser a b c d
   pDynE :: ParsRec a b c d -> AnaParser a b c d
   handleEof :: InputState a b => a b -> Steps (Pair (a b) c) b
   parse :: InputState a b => AnaParser a Pair b c -> a b -> Steps (Pair c (Pair (a b) d)) b
   parseIO :: InputState a b => AnaParser a Pair b c -> a b -> IO c
   parsebasic :: InputState a b => ParsRec a Pair b c -> a b -> Steps (Pair c (Pair (a b) d)) b
   parsebasic (PR ( P rp, _))
    = rp Pair handleEof

   parseIO (pp) inp
    = do  (Pair v final) <- evalStepsIO (parsebasic (pars pp) inp) 
          final `seq` return v -- in order to force the trailing error messages to be printed

   parse (pp)
    = parsebasic (pars pp) 

   handleEof input = case splitStateE input
                      of Left'  s  ss  ->  StRepair (deleteCost s)  (Msg ("deleting symbol " ++ show s
                                                                         , "in unused part of input"
                                                                         , EStr "eof"
                                                                         )) (handleEof ss)
                         Right' final  ->  NoMoreSteps (Pair final undefined)





   infixl 2 <?>
   infixl 3 <|>
   infixl 4 <*>, <$> , <+>
   infixl 4 ~*~,  ~$~

   infixl 4 <$, <*, *>, <**>, <??>
   infixl 2 `opt`
   infixl 5 <..>





   type Parser = AnaParser []  Pair 

   data Pair a r = Pair a r

   data Either' state s = Left' s (state s)
                        | Right' (state s)

   instance Symbol s => InputState [] s where
    splitStateE []     = Right' []
    splitStateE (s:ss) = Left'  s ss
    splitState  (s:ss) = ({-L-} s, ss{-R-})
    firstState  []     = Nothing
    firstState  (s:ss) = Just s
    getPosition []     = "unexpected end of input"
    getPosition (s:ss) = "before " ++ show s
    {-# INLINE splitStateE #-}
    {-# INLINE splitState  #-}
    
   instance OutputState Pair  where
     acceptR            = Pair
     nextR       acc  f   ~(Pair a r) = acc  (f a) r  
     dollarR     acc  f  v = acc  (f v)
     {-# INLINE acceptR #-}
     {-# INLINE nextR   #-}
     {-# INLINE dollarR #-}

   instance (Symbol s, InputState state s, OutputState result) => Sequence (AnaParser state result s)    where
     (<*>) = anaSeq libDollar  libSeq  ($) 
     (<* ) = anaSeq libDollarL libSeqL const
     ( *>) = anaSeq libDollarR libSeqR (flip const) 
     pSucceed = anaSucceed
     pLow     = anaLow

   instance (Symbol s, InputState state s, OutputState result) => Alternative (AnaParser state result s) where
     (<|>) = anaOr
     pFail = anaFail

   instance (Symbol s, InputState state s, OutputState result) => SymParser (AnaParser state result s) s where
     pCostRange   = anaCostRange
     pCostSym     = anaCostSym
     getfirsts    = anaGetFirsts
     setfirsts    = anaSetFirsts

   instance (InputState state s, OutputState result, Symbol s) => SplitParser (AnaParser state result s) where
    getzerop  p = case zerop p of
                    Nothing     -> Nothing
                    Just (b,e)  -> Just p {pars=libSucceed `either` id $ e
                                          ,onep=noOneParser
                                          }
    getonep   p = let tab = table (onep p)
                  in if null tab then Nothing else Just (mkParser Nothing (onep p))

   pDynE = anaDynE
   pDynL = anaDynL
   pDynN = anaDynN





   class    (Sequence p, Alternative p, SymParser p s, SplitParser p,  Show s) => IsParser p s | p -> s

   instance (Sequence p, Alternative p, SymParser p s, SplitParser p,  Show s) => IsParser p s

   class Sequence p where
     (<*>) :: p (a->b) -> p a -> p b
     (<* ) :: p  a     -> p b -> p a
     ( *>) :: p  a     -> p b -> p b
     (<$>) ::   (a->b) -> p a -> p b
     (<$ ) ::   f      -> p a -> p f
     pSucceed :: a -> p a
     pLow :: a -> p a
     f <$> p = pSucceed f <*> p
     f <$  q = pSucceed f <*  q
     p <*  q = pSucceed       const  <*> p <*> q
     p  *> q = pSucceed (flip const) <*> p <*> q

   class Alternative p where
     (<|>) :: p a -> p a -> p a
     pFail :: p a

   class SymParser p s | p -> s where
    pCostRange :: Int{-I-} -> s -> SymbolR s -> p s
    pCostSym :: Int{-I-} -> s -> s         -> p s
    pSym ::             s         -> p s
    pRange ::        s -> SymbolR s -> p s
    getfirsts ::  p v -> Exp s
    setfirsts ::  Exp s -> p v ->  p v
    pSym a       =  pCostSym   5{-I-} a a
    pRange       =  pCostRange 5{-I-}

   class SplitParser p where
    getzerop ::  p v -> Maybe (p v)
    getonep ::  p v -> Maybe (p v)

   class Symbol s => InputState state s where
    splitStateE :: state s            -> Either' state s
    splitState :: state s            -> ({-L-} s, state s {-R-})
    firstState :: state s            -> Maybe s
    getPosition :: state s            -> String

   class OutputState r  where
     acceptR ::                              v             -> rest        -> r v rest
     nextR ::    (a ->     rest -> rest') -> (b -> a)      -> r b rest  -> rest'
     dollarR ::  (a -> r c rest -> rest') -> (b -> a) -> b -> r c rest  -> rest'

   class (Ord s, Show s) => Symbol s where
    deleteCost :: s -> Int{-I-}
    symBefore :: s -> s
    symAfter :: s -> s
    deleteCost b = 5{-I-}
    symBefore  = fatal 398 "You should have made your token type an instance of the Class Symbol. eg by defining symBefore = pred"
    symAfter   = fatal 399 "You should have made your token type an instance of the Class Symbol. eg by defining symAfter  = succ"





   type Result val s = Steps val s

   newtype RealParser    state result s a = P(forall r r' b. (a -> result b r -> r') ->
                                                             (state s -> Result (result b r) s) ->  state s -> Result r' s)

   newtype RealRecogn    state        s   = R(forall r     . (state s -> Result           r  s) ->  state s -> Result r  s)

   newtype ParsRec       state result s a = PR  ( RealParser  state result s a
                                                , RealRecogn  state        s
                                                )

   {-# INLINE unP #-}
   {-# INLINE unR #-}
   unP  (P  p) = p
   unR  (R  p) = p





   libAccept            =  PR  (P (\ acc k state ->
                                   case splitState state of
                                   ({-L-} s, ss {-R-})  -> OkVal (acc s) (k ss))
                               ,R (\ k state ->
                                   case splitState state of
                                   ({-L-} s, ss {-R-})  ->   Ok (k ss))
                               )
   libInsert  c sym  firsts = PR ( P (\acc k state ->  StRepair c (Msg  ("inserting symbol " ++ show sym
                                                                        , getPosition state
                                                                        , firsts
                                                                        )) (val (acc sym) (k state)))
                                 , R (\    k state ->  StRepair c (Msg  ("inserting symbol " ++ show sym
                                                                        , getPosition state
                                                                        , firsts
                                                                        ))                (k state))
                                 )
   {-# INLINE libSeq  #-}
   {-# INLINE libSeqL #-}
   {-# INLINE libSeqR #-}
   {-# INLINE libDollar #-}
   {-# INLINE libDollarL #-}
   {-# INLINE libDollarR #-}
   {-# INLINE libSucceed #-}

   libSucceed v                                 = PR ( P (\ acc -> let accv = val (acc v) in \ k state -> accv (k state))
                                                     , R id
                                                     )
   libSeq  (PR (P pp, R pr)) ~(PR (P qp, R qr)) = PR ( P (\ acc -> pp (nextR acc).qp acceptR)
                                                     , R (pr.qr)
                                                     )
   libDollar f                (PR (P qp, R qr)) = PR ( P (\ acc -> qp (dollarR acc f))
                                                     , R qr
                                                     )
   libDollarL f               (PR (P qp, R qr)) = PR ( P (\ acc -> let accf = val (acc f) in \ k -> qr (accf . k))
                                                     , R qr
                                                     )
   libDollarR f               (PR (P qp, R qr)) = PR (P qp, R qr)

   libSeqL (PR (P pp, R pr)) ~(PR (P qp, R qr)) = PR  ( P (\acc -> pp acc.qr)
                                                      , R(pr.qr)
                                                      )
   libSeqR (PR (P pp, R pr)) ~(PR (P qp, R qr)) = PR  ( P (\acc -> pr.qp acc )
                                                      , R(pr.qr)
                                                      )
   libOr   (PR (P pp, R pr))  (PR (P qp, R qr)) = PR  ( P (\ acc -> let p = pp acc
                                                                        q = qp acc
                                                                    in \ k state   -> p  k state `libBest` q  k state)
                                                      , R (\             k state   -> pr k state `libBest` qr k state)
                                                      )
   libFail                                      = PR ( P (\ _ _  _  -> (usererror  "calling an always failing parser"    ))
                                                     , R (\   _  _  -> (usererror  "calling an always failing recogniser"))
                                                     )
         





   data Steps val s 
                = forall a . OkVal           (a -> val)                             (Steps a   s)
                |            Ok         {                                    rest :: Steps val s}
                |            Cost       {costing::Int{-I-}                 , rest :: Steps val s}
                |            StRepair   {costing::Int{-I-}, m :: Message s , rest :: Steps val s}
                | forall v w.Best       (Steps v s) (Steps val s) (Exp s) ( Steps w s)
                |            NoMoreSteps val
   val f (OkVal a rest) = OkVal (f.a) rest
   val f (Ok      rest) = OkVal  f rest
   val f (Cost i  rest) = Cost i (val f rest)
   val f (StRepair c m r) = StRepair c m (val f r)
   val f (Best l s e r)   = Best l (val f s) e r
   val f (NoMoreSteps v)  = NoMoreSteps (f v)

   starting (StRepair _ m _ ) = getStart m
   starting (Best _ _ s _ )   = s
   starting _                 = systemerror "UU_Parsing" "starting"

   hasSuccess (OkVal _ _ ) = True
   hasSuccess (Ok      _ ) = True
   hasSuccess (NoMoreSteps _) = True
   hasSuccess (Cost i  _    ) = True
   hasSuccess _               = False

   evalSteps (OkVal v  rest    ) = v (evalSteps rest)
   evalSteps (Ok       rest    ) =    evalSteps rest
   evalSteps (Cost  _  rest    ) =    evalSteps rest
   evalSteps (StRepair _ msg rest    ) =    evalSteps rest
   evalSteps (Best _   rest _ _) =  evalSteps rest
   evalSteps (NoMoreSteps v    ) =  v

   evalStepsIO (OkVal v  rest    ) = do arg <- unsafeInterleaveIO (evalStepsIO rest)
                                        return (v arg)
   evalStepsIO (Ok       rest    ) = evalStepsIO rest

   evalStepsIO (Cost  _  rest    ) = evalStepsIO rest
   evalStepsIO (StRepair _ msg rest    ) = do putStr (fatal 519 (show msg)) -- was: do putStr (show msg) -- b.joosten
                                              evalStepsIO rest
   evalStepsIO (Best _   rest _ _) =  evalStepsIO rest
   evalStepsIO (NoMoreSteps v    ) =  return v

   getMsgs (OkVal _        rest) = getMsgs rest
   getMsgs (Ok             rest) = getMsgs rest
   getMsgs (Cost _         rest) = getMsgs rest
   getMsgs (StRepair _ m   rest) = m:getMsgs rest
   getMsgs (Best _ m _ _)        = getMsgs m
   getMsgs (NoMoreSteps _      ) = []

   newtype Message s  =  Msg (String, String, Exp s) -- action, position, expecting 
                         deriving Eq
   getStart (Msg (_,_,st)) = st

   addToMessage (Msg (act, pos, exp)) more = Msg (act, pos, more `eor` exp)

   marks = '\n':take 60 qmarks
           where qmarks = '?':qmarks

   instance Symbol s => Show (Message s) where
    show (Msg (action, position, expecting))  
      =  "\n" ++ position ++
         "\nExpecting " ++ show expecting ++
         "\nTry " ++ action ++ "\n"

   addexpecting more  (StRepair    cost   msg   rest) = StRepair cost (addToMessage msg more) rest
   addexpecting more  (Best     l    sel  starting r) = Best l (addexpecting more sel) starting r
   addexpecting more  (OkVal v rest                 ) =  systemerror "UU_Parsing" "addexpecting: OkVal"
   addexpecting more  (Ok   _                       ) =  systemerror "UU_Parsing" "addexpecting: Ok"
   addexpecting more  (Cost _ _                     ) =  systemerror "UU_Parsing" "addexpecting: Cost"
   addexpecting more  _                               =  systemerror "UU_Parsing" "addexpecting: other"
   data Exp s = ESym (SymbolR s)
              | EStr String
              | EOr  [Exp s]
              | ESeq [Exp s]
              deriving (Ord, Eq)

   eor p  q  = EOr (merge (tolist p) (tolist q))
               where merge x@(l:ll) y@(r:rr) = case compare l r of
                                               LT -> l:( ll `merge`  y)
                                               GT -> r:( x  `merge` rr)
                                               EQ -> l:( ll `merge` rr)
                     merge l [] = l
                     merge [] r = r
                     tolist (EOr l) = l
                     tolist x       = [x]

   instance Symbol s => Show (Exp s) where
    show (ESym     s)   = show s
    show (EStr   str)   = str
    show (EOr     [])   = "Nothing expected "
    show (EOr    [e])   = show e
    show (EOr  (e:ee))  = show e ++ " or " ++ show (EOr ee)
    show (ESeq  seq)    = concatMap show seq






   libBest ls rs = libBest' ls rs id id
   libBest' (OkVal v ls) (OkVal w rs) lf rf = Ok (libBest' ls rs (lf.v) (rf.w))
   libBest' (OkVal v ls) (Ok      rs) lf rf = Ok (libBest' ls rs (lf.v)  rf   )
   libBest' (Ok      ls) (OkVal w rs) lf rf = Ok (libBest' ls rs  lf    (rf.w))
   libBest' (Ok      ls) (Ok      rs) lf rf = Ok (libBest' ls rs  lf     rf   )
   libBest' (OkVal v ls) _            lf rf = OkVal (lf.v) ls 
   libBest' _            (OkVal w rs) lf rf = OkVal (rf.w) rs 
   libBest' (Ok      ls) _            lf rf = OkVal lf ls           
   libBest' _            (Ok      rs) lf rf = OkVal rf rs   
   libBest' l@(Cost i ls ) r@(Cost j rs ) lf rf
    | i =={-I-} j = Cost i (libBest' ls rs lf rf)
    | i <{-I-} j  = Cost i (val lf ls)
    | i >{-I-} j  = Cost j (val rf rs)
   libBest' l@(Cost i ls)     _                 lf rf = Cost i (val lf ls)
   libBest' _                 r@(Cost j rs)     lf rf = Cost j (val rf rs)
   libBest' l@(NoMoreSteps v) _                 lf rf = NoMoreSteps (lf v)
   libBest' _                 r@(NoMoreSteps w) lf rf = NoMoreSteps (rf w)
   libBest' l                 r                 lf rf = libCorrect l r lf rf

   libCorrect ls rs lf rf
    =  let (Pairs  _ select) = traverse (traverse (Pairs 999{-I-} fst) (Pairs 0{-I-} fst) ls 4{-I-})  (Pairs 0{-I-} snd) rs 4{-I-} 
           leftstart  = starting ls
           rightstart = starting rs
       in Best ls
               (select (val lf (addexpecting rightstart ls), val rf (addexpecting leftstart rs)))
               (leftstart `eor` rightstart)
               rs

   data Pairs = Pairs Int{-I-} (forall a. (a,a) -> a)
   traverse b@(Pairs bv br) t@(Pairs tv tr) _                             0{-I-}  = if bv <{-I-} tv then b else t
   traverse b@(Pairs bv br) t@(Pairs tv tr) (Ok            l)             n       = traverse b t l (n -{-I-} 1{-I-})
   traverse b@(Pairs bv br) t@(Pairs tv tr) (OkVal  v      l)             n       = traverse b t l (n -{-I-} 1{-I-})
   traverse b@(Pairs bv br) t@(Pairs tv tr) (Cost i   l)                  n       = if i +{-I-} tv >={-I-} bv then b else traverse b (Pairs (i +{-I-} tv) tr) l (n -{-I-} 1{-I-})
   traverse b@(Pairs bv br) t@(Pairs tv tr) (Best l _ _ r)                n = traverse (traverse b t l n) t r n
   traverse b@(Pairs bv br) t@(Pairs tv tr) (StRepair     i   msgs     r) n = if i +{-I-} tv >={-I-} bv then b else traverse b (Pairs (i +{-I-} tv) tr) r (n -{-I-} 1{-I-})
   traverse b@(Pairs bv br) t@(Pairs tv tr) (NoMoreSteps _)               n = if bv <{-I-} tv then b else t





   data AnaParser  state result s a
    = AnaParser { pars :: ParsRec state result s a
                , zerop :: Maybe (Bool, Either a (ParsRec state result s a))
                , onep :: OneDescr state  result s a
                } -- deriving Show
   data OneDescr  state result s a
    = OneDescr  { leng :: Nat
                , firsts :: Exp s
                , table :: [(SymbolR s, TableEntry state result s a)]
                } -- deriving Show
                
   data TableEntry state result s a = TableEntry (ParsRec  state result s a) (Exp s -> ParsRec state result s a)





   anaFail = AnaParser { pars    = libFail
                       , zerop   = Nothing
                       , onep    = noOneParser
                       }
   noOneParser = OneDescr Infinite (EOr []) []

   pEmpty p zp = AnaParser { pars    = p
                           , zerop   = Just zp
                           , onep    = noOneParser
                           }

   anaSucceed  v = pEmpty (libSucceed v) (False, Left v)
   anaLow      v = pEmpty (libSucceed v) (True,  Left v)
   anaDynE     p = pEmpty p              (False, Right p)
   anaDynL     p = pEmpty p              (True , Right p)
   anaDynN  fi len range p = mkParser  Nothing (OneDescr len fi [(range, p)]) 

   anaOr ld@(AnaParser _ zl ol)  rd@(AnaParser _ zr or)
    = mkParser newZeroDescr newOneDescr
      where newZeroDescr  = case zl of {Nothing -> zr
                                       ;_       -> case zr of {Nothing -> zl
                                                              ;_       -> usererror ("Two empty alternatives, where expecting"++show (firsts newOneDescr))
                                       }                      }
            newOneDescr   =  orOneOneDescr ol or False

   {-# INLINE anaSeq #-}
   anaSeq libdollar libseq comb (AnaParser  pl zl ol)  ~rd@(AnaParser pr zr or)
    = case zl of
      Just (b, zp ) -> let newZeroDescr = seqZeroZero zl zr   libdollar libseq comb
                           newOneDescr = let newOneOne  = mapOnePars (   `libseq` pr) (const Infinite) ol
                                             newZeroOne = case zp of
                                                          Left  f -> mapOnePars (f `libdollar`   ) id or
                                                          Right p -> mapOnePars (p `libseq`      ) id or
                                         in orOneOneDescr newZeroOne newOneOne  b -- left one is shortest
                       in mkParser newZeroDescr newOneDescr
      _            ->  AnaParser  (pl `libseq` pr) Nothing  (mapOnePars (`libseq` pr) (`nat_add` (pLength rd)) ol)

   seqZeroZero Nothing             _                    _          _      _   = Nothing
   seqZeroZero _                   Nothing              _          _      _   = Nothing 
   seqZeroZero (Just (llow, left)) (Just (rlow, right))  libdollar libseq comb
       = Just      ( llow || rlow
                  , case left of
                    Left  lv  -> case right of
                                 Left  rv -> Left (comb lv rv)
                                 Right rp -> Right (lv `libdollar` rp)
                    Right lp  -> case right of
                                 Left  rv  -> Right (lp `libseq` libSucceed rv)
                                 Right rp  -> Right (lp `libseq` rp)
                  )

   orOneOneDescr ~(OneDescr ll fl tl) ~(OneDescr lr fr tr)  b
                     = let newfirsts       = (fl `eor` fr) 
                           (newlength, maybeswap) = ll `nat_min` lr
                           (tla, tra)      = if b then (tl, tr) else maybeswap (tl, tr)
                           keystr          = map fst tra
                           lefttab         = if b then [r | r@(k,_) <- tla, k `notElem` keystr] else tla
                       in OneDescr newlength (fl `eor` fr) (lefttab ++ tra)

   anaCostRange _        _     EmptyR = anaFail
   anaCostRange ins_cost ins_sym range
     = mkParser Nothing ( OneDescr (Succ Zero) (ESym range) [(range, TableEntry  libAccept 
                                                                                 (libInsert ins_cost ins_sym)
                                                            )]) 

   anaCostSym   i ins sym = pCostRange i ins (Range sym sym)

   pLength (AnaParser _ (Just _)  _ ) = Zero
   pLength (AnaParser _  Nothing  od) = leng od

   anaGetFirsts (AnaParser  p z od) = firsts od

   anaSetFirsts newexp (AnaParser  _ zd od)
    = mkParser zd (od{firsts = newexp })





   mapOnePars fp fl ~(OneDescr l fi t) = OneDescr (fl l) fi [ (k, TableEntry (fp p) (fp.corr))
                                                            | (k, TableEntry     p      corr ) <- t
                                                            ]





   mkParser  zd ~descr@(OneDescr _ firsts tab) -- pattern matching should be lazy for lazy computation of length for empty parsers
    = let parstab    = if null tab then fatal 726 "parstab undefined" else
                       foldr1 mergeTables  [[(k, p)] | (k, TableEntry p _) <- tab]
          mkactualparser getp 
            = let find       = case  parstab of
                               [(ran,  pp)]     -> let comp = symInRange ran
                                                       pars = Just (getp pp) 
                                                   in \ s -> if comp s then pars else Nothing
                               _           -> btLookup.tab2tree $  [(k,Just (getp pr) ) | (k, pr) <- parstab]
                  zerop      = getp (case zd of
                                    Nothing           -> libFail
                                    Just (_, Left v)  -> libSucceed v
                                    Just (_, Right p) -> p
                                    ) 
                  insertsyms = if null tab then fatal 739 "insertsyms undefined" else
                               foldr1 lib_correct [   getp (pr firsts) | (_ , TableEntry _ pr) <- tab    ]
                  correct k inp
                    = case splitState inp of
                          ({-L-} s, ss {-R-}) -> libCorrect (StRepair(deleteCost s) (Msg  ("deleting symbol " ++ show s
                                                                                          , getPosition inp
                                                                                          , firsts
                                                                                    )     ) (result k ss)) 
                                                            (insertsyms k inp) id id
                  result = if null tab then zerop
                           else case zd of
                           Nothing        ->(\k inp -> case splitStateE inp of
                                                       Left' s ss -> case find s of 
                                                                     Just p  ->  p k inp
                                                                     Nothing -> correct k inp
                                                       Right' _   -> insertsyms   k inp)
                           Just (True, _) ->(\k inp -> case splitStateE inp of
                                                       Left' s ss -> case find s of 
                                                                     Just p  -> p k inp 
                                                                     Nothing -> let r = zerop k inp 
                                                                                in if hasSuccess r then r else libCorrect r (correct k inp) id id
                                                       Right' _   -> zerop k inp)
                           Just (False, _) ->(\k inp -> case splitStateE inp of
                                                       Left' s ss -> case find s of 
                                                                     Just p  -> p k inp `libBest` zerop k inp
                                                                     Nothing -> let r = zerop k inp 
                                                                                in if hasSuccess r then r else libCorrect r (correct k inp) id id
                                                       Right' _   -> zerop k inp)
              in result
          res    = PR (P ( \ acc ->  mkactualparser (\ (PR (P p, _)) -> p acc))
                      ,R (           mkactualparser (\ (PR (_, R p)) -> p    ))
                      )            
      in AnaParser res zd descr
      
   lib_correct p q k inp = libCorrect (p k inp) (q k inp) id id





   data Nat = Zero
            | Succ Nat
            | Infinite
            deriving (Eq, Show)

   nat_le Zero      _        = True
   nat_le _         Zero     = False
   nat_le Infinite  _        = False
   nat_le _         Infinite = True
   nat_le (Succ l) (Succ r) = nat_le l r

   nat_min Infinite   r          = (r, swap) where swap (a,b) = (b,a)
   nat_min l          Infinite   = (l, id)
   nat_min Zero       _          = (Zero, id)
   nat_min _          Zero       = (Zero, swap) where swap (a,b) = (b,a)
   nat_min (Succ ll)  (Succ rr)  = let (v, fl) = ll `nat_min` rr in (Succ v, fl)

   nat_add Infinite  _ = Infinite
   nat_add Zero      r = r
   nat_add (Succ l)  r = Succ (nat_add l r)





   mergeTables l []  = l
   mergeTables [] r  = r
   mergeTables lss@(l@(le@(Range a b),ct ):ls) rss@(r@(re@(Range c d),ct'):rs)
    = let ct'' =  ct `libOr` ct'
      in  if      c<a then   mergeTables rss lss     -- swap
          else if b<c then l:mergeTables ls  rss     -- disjoint case
          else if a<c then (Range a (symBefore c),ct) :mergeTables ((Range c b,ct):ls)             rss
          else if b<d then (Range a b,ct'')           :mergeTables ((Range (symAfter b) d,ct'):rs) ls
          else if b>d then mergeTables rss lss
                      else (le,ct'') : mergeTables ls rs-- equals





   libMap ::    (forall r r'' . (b -> r -> r'') -> state s -> Result (a, r) s -> ( state s, Result  r'' s)) 
             -> (forall r      .                   state s -> Result     r  s -> ( state s, Result  r   s))
             -> ParsRec state result s a -> ParsRec state result s b
   libMap f f' (PR (P p, R r))       = PR ( P(\acc -> let pp   = p (,)
                                                          facc = f acc 
                                                      in \ k instate  -> let inresult = pp k outstate
                                                                             (outstate, outresult) = facc instate inresult
                                                                         in outresult
                                             )
                                          , R(\ k instate  -> let inresult = r k outstate
                                                                  (outstate, outresult) = f' instate inresult
                                                              in outresult)
                                          )

   pMap ::    OutputState result =>
                (forall r r'' . (b -> r -> r'') -> state s -> Result (a, r) s -> ( state s, Result r'' s)) 
             -> (forall r     .                    state s -> Result     r  s -> ( state s, Result r   s))
             ->  AnaParser state result s a -> AnaParser state result s b

   pMap f f'  (AnaParser p z o) = AnaParser (libMap f f' p)
                                             (case z of
                                              Nothing     -> Nothing
                                              Just (b, v) -> Just (b, case v of
                                                                      Left w   -> Right (libMap f f' (libSucceed w))
                                                                      Right pp -> Right (libMap f f' pp)))
                                             (mapOnePars (libMap f f') id o)
   libWrap ::    (forall r r'' .   (b -> r -> r'') 
                                       -> state s 
                                       -> Result (a, r) s 
                                       -> (state s -> Result r s) 
                                       -> (state s, Result r'' s, state s -> Result r s))
              -> (forall r         .   state s 
                                   -> Result r  s 
                                   -> (state s -> Result r s) 
                                   -> (state s, Result r s, state s -> Result r s)) 
              -> ParsRec state result s a -> ParsRec state result s b
   libWrap f f' (PR (P p, R r)) = PR ( P(\ acc -> let pp = p (,)
                                                      facc = f acc
                                                  in \ k instate  -> let (stl, ar, str2rr) = facc instate rl k
                                                                         rl                = pp str2rr stl
                                                                     in  ar
                                        )
                                     , R(\ k instate  -> let (stl, ar, str2rr) = f' instate rl k
                                                             rl                = r str2rr stl
                                                         in  ar)
                                     )

   pWrap ::    OutputState result 
              => (forall r  r''.   (b -> r -> r'') 
                                       -> state s 
                                       -> Result (a, r) s 
                                       -> (state s -> Result r s) 
                                       -> (state s, Result r'' s, state s -> Result r s))
              -> (forall r         .   state s 
                                   -> Result r s 
                                   -> (state s -> Result r s) 
                                   -> (state s, Result r s, state s -> Result r s)) 
              -> AnaParser state result s a -> AnaParser state result s b

   pWrap f f'  (AnaParser p z o) = AnaParser (libWrap f f' p)
                                             (case z of
                                              Nothing     -> Nothing
                                              Just (b, v) -> Just (b, case v of
                                                                      Left w   -> Right (libWrap f f' (libSucceed w))
                                                                      Right pp -> Right (libWrap f f' pp)))
                                             (mapOnePars (libWrap f f') id o)





   data  SymbolR s  =  Range s s | EmptyR deriving (Eq,Ord)

   instance Symbol s => Show (SymbolR s) where
    show EmptyR      = "the empty range"
    show (Range a b) = if a == b then show a else show a ++ ".." ++ show b

   mk_range             l    r =  if l > r then EmptyR else Range l r

   symInRange (Range l r) = if l == r then (l==)
                                      else (\ s -> not (s < l || r < s ))

   symRS (Range l r)
     = if l == r then (compare l)
       else (\ s -> if      s < l then GT
                    else if s > r then LT
                    else               EQ)

   range `except` elems
    = foldr removeelem [range] elems
      where removeelem elem ranges = [r | ran <- ranges, r <- ran `minus` elem]
            EmptyR          `minus` _    = []
            ran@(Range l r) `minus` elem = if symInRange ran elem
                                           then [mk_range l (symBefore elem), mk_range (symAfter elem) r]
                                           else [ran]





   usererror   m = fatal 917 $ "Your grammar contains a problem:\n" ++ m
   systemerror modname m
     = fatal 919 $ "I apologise: I made a mistake in my design. This should not have happened.\n" ++
                   " Please report: " ++ modname ++": " ++ m ++ " to doaitse@cs.uu.nl\n"





   newtype Perms p a = Perms (Maybe (p a), [Br p a])
   data Br p a = forall b. Br (Perms p (b -> a)) (p b)

   perms ~*~ p = perms `add` (getzerop p, getonep p)
   f     ~$~ p = Perms (Just (pLow f), []) ~*~ p

   add b2a@(Perms (eb2a, nb2a)) bp@(eb, nb)
    =  let changing :: Sequence a => (b -> c) -> Perms a b -> Perms a c
           f `changing` Perms (ep, np) = Perms (fmap (f <$>) ep, [Br ((f.) `changing` pp) p | Br pp p <- np])
       in Perms
         ( do { f <- eb2a
              ; x <- eb
              ; return (f <*>  x)
              }
         ,  (case nb of
             Nothing     -> id
             Just pb     -> (Br b2a  pb:)
           )[ Br ((flip `changing` c) `add`  bp) d |  Br c d <- nb2a]
         )

   pPerms (Perms (empty,nonempty))
    = foldl (<|>) (fromMaybe pFail empty) [ (flip ($)) <$> p <*> pPerms pp
                                          | Br pp  p <- nonempty
                                          ]

   pPermsSep  = p2p (pSucceed ()) 

   p2p fsep sep (Perms (mbempty, nonempties)) = foldr (<|>) empty (map pars nonempties)
    where empty      = fromMaybe  pFail mbempty
          pars (Br t p)  = flip ($) <$ fsep <*> p <*> p2p sep sep t





   acceptsepsilon p       = case getzerop p of {Nothing -> False; _ -> True}

   mnz p v
      = if( acceptsepsilon p)
        then   usererror ("You are calling a list based derived combinator with a parser that accepts the empty string.\n"
                       ++
                      "We cannot handle the resulting left recursive formulation (and it is ambiguous too).\n"++
                      (case getfirsts p of
                       ESeq []  ->  "There are no other alternatives for this parser"
                       d        ->  "The other alternatives of this parser may start with:\n"++ show d
                     ))
        else v





   a <..> b   = pRange a (Range a b)
   (l,r,err) `pExcept` elems = let ranges = filter (/= EmptyR) (Range l r `except` elems)
                               in if null ranges then pFail
                                  else foldr (<|>) pFail (map (pRange err) ranges)

   p `opt` v       = mnz p (p  <|> pLow v)   -- note that opt is greedy, if you do not want this
                                                 -- use "... <|> pSucceed v"  instead
                                                 -- p should not recognise the empty string





   asList  exp = setfirsts (ESeq [EStr "(",  exp, EStr  " ...)*"])
   asList1 exp = setfirsts (ESeq [EStr "(",  exp, EStr  " ...)+"])
   asOpt   exp = setfirsts (ESeq [EStr "( ", exp, EStr  " ...)?"])
   pa <+> pb       = (,) <$> pa <*> pb
   p <**> q        = (\ x f -> f x) <$> p <*> q
   f <$$> p        = pSucceed (flip f) <*> p
   p <??> q        = p <**> (q `opt` id)
   p <?>  str      = setfirsts  (EStr str) p
   pPacked l r x   =   l *>  x <*   r





   pFoldr_ng      alg@(op,e)     p = mnz p (asList (getfirsts p) pfm)
                                     where pfm = (op <$> p <*> pfm)  <|> pSucceed e
   pFoldr_gr      alg@(op,e)     p = mnz p (asList (getfirsts p) pfm)
                                     where pfm = (op <$> p <*> pfm) `opt` e
   pFoldr                          = pFoldr_gr

   pFoldr1_gr     alg@(op,e)     p = asList1 (getfirsts p) (op <$> p <*> pFoldr_gr  alg p)
   pFoldr1_ng     alg@(op,e)     p = asList1 (getfirsts p) (op <$> p <*> pFoldr_ng  alg p)
   pFoldr1                         = pFoldr1_gr

   pFoldrSep_gr   alg@(op,e) sep p = mnz p (asList (getfirsts p)((op <$> p <*> pFoldr_gr alg (sep *> p)) `opt` e ))
   pFoldrSep_ng   alg@(op,e) sep p = mnz p (asList (getfirsts p)((op <$> p <*> pFoldr_ng alg (sep *> p))  <|>  pSucceed e))
   pFoldrSep                       = pFoldrSep_gr

   pFoldr1Sep_gr  alg@(op,e) sep p = if acceptsepsilon sep then mnz p pfm else pfm
                                     where pfm = op <$> p <*> pFoldr_gr alg (sep *> p)
   pFoldr1Sep_ng  alg@(op,e) sep p = if acceptsepsilon sep then mnz p pfm else pfm
                                     where pfm = op <$> p <*> pFoldr_ng alg (sep *> p)
   pFoldr1Sep                      = pFoldr1Sep_gr

   list_alg = ((:), [])

   pList_gr        = pFoldr_gr     list_alg  
   pList_ng        = pFoldr_ng     list_alg  
   pList           = pList_gr

   pList1_gr       = pFoldr1_gr    list_alg  
   pList1_ng       = pFoldr1_ng    list_alg  
   pList1          = pList1_gr               

   pListSep_gr     = pFoldrSep_gr  list_alg
   pListSep_ng     = pFoldrSep_ng  list_alg
   pListSep        = pListSep_gr

   pList1Sep_gr    = pFoldr1Sep_gr list_alg
   pList1Sep_ng    = pFoldr1Sep_ng list_alg
   pList1Sep       = pList1Sep_gr

   pChainr_gr op x    =  if acceptsepsilon op then mnz x r else r
                      where r = x <??> (flip <$> op <*> r)
   pChainr_ng op x    =  if acceptsepsilon op then mnz x r else r
                      where r = x <**> ((flip <$> op <*> r)  <|> pSucceed id)
   pChainr            = pChainr_gr

   pChainl_gr op x    =  if acceptsepsilon op then mnz x r else r
                         where
                          r      = (f <$> x <*> pList_gr (flip <$> op <*> x) )
                          f x [] = x
                          f x (func:rest) = f (func x) rest

   pChainl_ng op x    =  if acceptsepsilon op then mnz x r else r
                      where
                       r      = (f <$> x <*> pList_ng (flip <$> op <*> x) )
                       f x [] = x
                       f x (func:rest) = f (func x) rest
   pChainl            = pChainl_gr

   pAny  f l = if null l then usererror "pAny: argument may not be empty list" else foldr1 (<|>) (map f l)
   pAnySym   = pAny pSym -- used to be called pAnySym






   (pe, pp, punp) <||> (qe, qp, qunp)
    =( (pe, qe)
     , (\f (pv, qv) -> (f pv, qv)) <$> pp
                 <|>
       (\f (pv, qv) -> (pv, f qv)) <$> qp
     , \f (x, y) -> qunp (punp f x) y
     )

   sem `pMerged` (units, alts, unp)
    = let pres = alts <*> pres `opt` units
      in unp sem <$> pres

   usealg (op, e) p = (e, op <$> p, id)
   list_of = usealg list_alg

   pToks []     = pSucceed []
   pToks (a:as) = (:) <$> pSym a <*> pToks as

   pLocate = pAny pToks





   data BinSearchTree a b
    = Node (BinSearchTree a b) (a, b) (BinSearchTree a b)
    | Nil

   tab2tree tab = tree
    where
     (tree,[]) = sl2bst (length tab) [ (symRS k, v) | (k, v) <- tab]
     sl2bst 0 list     = (Nil   , list)
     sl2bst n list
      = let
         ll = (n - 1) `div` 2 ; rl = n - 1 - ll
         (lt,a:list1) = sl2bst ll list
         (rt,  list2) = sl2bst rl list1
        in (Node lt a rt, list2)



   btLookup
    = find_in
      where find_in  Nil = \i -> Nothing
            find_in (Node Nil (k,v) Nil)
             = (\i -> case k i of    { LT -> Nothing
                                     ; EQ ->  v
                                     ; GT -> Nothing
                                     })
            find_in (Node Nil (k,v) right)
             = (\i -> case k i of    { LT -> findright i
                                     ; EQ ->  v
                                     ; GT -> Nothing
                                     })
               where findright = find_in right
            find_in (Node left (k,v) Nil)
             = (\i -> case k i of    { LT -> Nothing
                                     ; EQ -> v
                                     ; GT -> findleft  i
                                     })
               where findleft  = find_in left
            find_in (Node left (k,v) right)
             = (\i -> case k i of    { LT -> findright i
                                     ; EQ -> v
                                     ; GT -> findleft  i
                                     })
               where findleft  = find_in left
                     findright = find_in right
