module Prototype.ShowCode (showCode) where
 import Prototype.CodeAuxiliaries(CodeVariable(..),CodeQuery(..),Statement(..))
 import Prototype.RelBinGenBasics(indentBlockBetween)
 
 
 showCode :: Int -- indentation
          -> [Statement] -- code
          -> String
 showCode _ [] = ""
 showCode i (x:xs)
  = (case x of
      Assignment{assignTo=to,query=q}
       -> "\n"++showAssignmentCode i to q
      Iteration{loopOver=lp,loopBy=lb,loopValue=lv,stcode=c}
       -> concat ["\n"++sps++"$"++cvname decl++"=array();"|decl<-postknowledge x,notElem (cvname decl) ("":map cvname (preknowledge x))]
          ++"\n"++sps
          ++"foreach($"++cvname lp++" as $"++cvname lb++"=>"++cvname lv++"){"
          ++showCode (i+2) c
          ++"\n"++sps++"}"
      Forget{} -> []
    )++showCode i xs
   where
     sps = take i (repeat ' ')
 -- | give the PHP code for an assignemt of some query to some variable
 showAssignmentCode :: Int -- ^ indentation
                    -> [CodeVariable] -- ^ the variable that will have a value after this
                    -> CodeQuery -- ^ the query
                    -> String -- ^ PHP code
 showAssignmentCode i var quer
  = case quer of
     SQLBinary{sqlquery=str}
      -> indentBlockBetween (sps ++ "$"++varname++" = DB_doquer_lookups('") "');" (lines str)
     PHPIntersect{}
      -> line "intersect"
     PHPJoin{}
      -> line "join"
     PHPIsectComp{}
      -> line "isectComp" 
     SQLComposed{sqlquery=str}
      -> indentBlockBetween (sps ++ "$"++varname++" = DB_doquer_lookups('") "');" (lines str)
     CQCompose {cqFrom=fm}
      -> indentBlockBetween (sps ++ "$"++varname++" = array") " );"
                            [ c:" '"++fst f++"' => $"++cvname (snd f)
                            | (c,f)<-zip ('(':repeat ',') fm
                            ]
     _ -> error ("cannot showCode in Assignment of query "++show quer ++"(inexhaustive patterns in Code.hs)")
    where
     varname = cvname (head var) ++ concat
               ["["++cvFullname++"]"|v'<-tail var,let cvFullname=case (cvname v') of {""->"";_->"$"++cvname v'}]
     sps=take i (repeat ' ')
     line str = sps ++ "$"++varname++"="++str++"($"++cvname (cqfrom1 quer)++",$"++cvname (cqfrom2 quer)++");"
 
