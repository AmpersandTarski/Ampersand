{-# OPTIONS_GHC -Wall #-}
module TypeInference.Input (allPatRules,allCtxPats,removeCtx) where
import Auxiliaries (eqCl)
import Collection (rd,uni)
import ADL  

   --DESCR -> removes a context from contexts
   --REMARK -> if a context is removed of which the name is not unique, then all the contexts with that name will be removed
   --          context names should be unique
removeCtx :: Contexts -> Context -> Contexts
-- WAAROM?? Waarom niet gewoon de Eq van Context gebruiken?? 
--          Het wordt dan : removeCtx ctxs cx = filter (/= cx) ctxs
-- was: removeCtx ctxs cx = [cx' | cx'<-ctxs, not((case cx of Ctx{} -> ctxnm cx) == (case cx' of Ctx{} -> ctxnm cx'))]
removeCtx ctxs cx = filter (/= cx) ctxs

--DESCR -> all the patterns of contexts
allCtxPats :: Contexts -> Patterns
allCtxPats ctxs = concat [ctxpats cx | cx<-ctxs]

 
--DESCR -> all the Rules of patterns
allPatRules :: Patterns -> Rules
allPatRules ps = concat [ptrls p | p<-ps]

allCtxCpts :: Contexts -> [Concept]
allCtxCpts ctxs
 = inject [ c { cptos = Just$rd (concat [atoms| (_,atoms)<-cl])  }
   | cl<-eqCl fst ([(source d,dom d (decpopu d))| d<-declarations ctxs]++[(target d,cod d (decpopu d))| d<-declarations ctxs]++
                   [(source pop,dom (popm pop) (popps pop))| pop<-pps]++[(target pop,cod (popm pop) (popps pop))| pop<-pps])
   , (c@C{},_)<-take 1 cl
   ] `uni` [S]
  where
   inject cs = cs ++ [x{cptos=Just []}|x@(C{})<-rd$concat[[gengen g,genspc g]|g<-gens ctxs], not$elem x cs] 
   pps = [ pop | cx<-ctxs, pop<-ctxpops cx]
   dom r ps = if isInj r then [ srcPaire p | p<-ps ] else rd [ srcPaire p | p<-ps ]
   cod r ps = if isUni r then [ trgPaire p | p<-ps ] else rd [ trgPaire p | p<-ps ]
 

