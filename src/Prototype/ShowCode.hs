{-# OPTIONS_GHC -Wall #-}
module Prototype.ShowCode (showCode,showCodeHeaders) where
 import Prototype.CodeStatement(Statement(..),CodeQuery(..),UseVar(..))
 import Prototype.CodeAuxiliaries(Named(..))
 import Prototype.RelBinGenBasics(indentBlockBetween)
 import qualified Data.Set as Set
 
 showCode :: Int -> [Statement] -> String
 showCode i s = fst (showCodeAndGetHeaders i s)
 showCodeHeaders :: [[Statement]] -> [String]
 showCodeHeaders s = (Set.toList (Set.unions (map snd (map (showCodeAndGetHeaders 0) s))))
 
 -- | combine CodeAndHeaders 
 (+++) :: (String,Set.Set String)->(String,Set.Set String)->(String,Set.Set String)
 (+++) (a,b) (c,d) = (a++c,Set.union b d)
 
 showCodeAndGetHeaders :: Int -- ^ indentation
          -> [Statement] -- ^ code
          -> (String,Set.Set String) -- ^ first is the PHP code, second is a set of functions and declarations it uses
 showCodeAndGetHeaders _ [] = ("",Set.empty)
 showCodeAndGetHeaders i (x:xs)
  = (case x of
      Assignment{assignTo=to,query=q}
       -> ("\n",Set.empty)+++showAssignmentCode i to q
      Iteration{loopOver=lp,loopBy=lb,loopValue=lv,stcode=c}
       -> (concat ["\n"++sps++"$"++nName decl++"=array();"|decl<-postknowledge x,notElem (nName decl) ("":map nName (preknowledge x))]
          ++"\n"++sps++"if(is_array("++showUseVar lp++"))"++"\n"++sps
          ++"foreach("++showUseVar lp++" as $"++nName lb++"=>$"++nName lv++"){"
          ,headersForUV lp)
          +++showCodeAndGetHeaders (i+2) c
          +++("\n"++sps++"}",Set.empty)
      Forget{} -> ("",Set.empty)
    )+++showCodeAndGetHeaders i xs
   where
     sps = take i (repeat ' ')

 headersForUV :: Named UseVar -> Set.Set String
 headersForUV _ = Set.empty

 showUseVar :: Named UseVar -> String
 showUseVar var
  = (if null (nName var) then "" else "$"++nName var) ++ (showUV (nObject var))
  where
   showUV :: UseVar -> String
   showUV (UseVar []) = ""
   showUV (UseVar (Left  s:xs)) = "[\""++s++"\"]"++(showUV (UseVar xs))
   showUV (UseVar (Right s:xs)) = "["++showUseVar s++"]"++(showUV (UseVar xs))

 -- | give the PHP code for an assignemt of some query to some variable
 showAssignmentCode :: Int -- ^ indentation
                    -> Named UseVar -- ^ the variable that will have a value after this
                    -> CodeQuery -- ^ the query
                    -> (String,Set.Set String) -- ^ fst is the PHP code, snd is a set of functions and declarations it uses
 showAssignmentCode i var quer
  =  (indentBlockBetween (sps ++ (showUseVar var)++" = ") ";" (lines$fst (assignment quer)),snd (assignment quer))
  where
   sps=take i (repeat ' ')
   assignment SQLBinary{sqlquery=str}   = (indentBlockBetween ("DB_doquer_lookups('") "')" (lines str),dbDoquer)
   assignment SQLComposed{sqlquery=str} = (indentBlockBetween ("DB_doquer_lookups('") "')" (lines str),dbDoquer)
   assignment CQCompose{cqFrom=fm}
    = (indentBlockBetween ("array") ")"
                          [ c:" '"++nName f++"' => "++fst(assignment (nObject f))
                          | (c,f)<-zip ('(':repeat ',') fm
                          ]
      , Set.unions [snd$assignment (nObject f)|f<-fm])
   assignment (CQPlain nuv) = ("@"++showUseVar nuv,headersForUV nuv)
   assignment rest =
     case rest of
     PHPIntersect{}
      -> line "intersect" (Set.singleton "function intersect($a,$b){$c=array();foreach($a as $i=>$v) if(isset($b[$i])){foreach($v as $v2) if(isElem($b[$i],$v2)){addOneTo($c[$i],$v2)}}; return $c;}"
                           `Set.union` isElem
                           `Set.union` addOneTo)
     PHPJoin{}
      -> line "joinvars" (Set.singleton "function joinvars($a,$b){$c=array();foreach($a as $i=>$v){foreach($v as $v2) $c[$i]=@$b[$v2];}return $c;}")
     PHPIsectComp{}
      -> line "isectComp" (Set.singleton "function isectComp($a,$b){$c=array();foreach($a as $i=>$v){if(!isset($b[$i])){$c[$i]=$v}else{foreach($v as $v2) if(!isElem($b[$i],$v2)){addOneTo($c[$i],$v2)}}} return $c;}"
                           `Set.union` isElem
                           `Set.union` addOneTo)
     PHPBinCheck{}
      -> foldr1 (+++)
                ([("(false==="++cqphpplug rest++"(",Set.empty)]++
                 [(prefix++str,sets)|(prefix,(str,sets))<-zip ("":repeat ",")$ map assignment (cqinput rest)]
                 ++[(")?array():array(",Set.empty)
                   ,assignment (fst$ cqreturn rest)
                   ,("=>array(",Set.empty)
                   ,assignment (snd$ cqreturn rest)
                   ,(")))",(case (cqphpfile rest) of
                                 Just str -> Set.singleton$ "require \""++str++"\";"
                                 Nothing -> Set.empty
                          )
                    )
                   ]
                 )
     PHPCompl1{} 
      -> ("(count(",Set.empty)+++(assignment$ cqfrom rest)+++
         (")?array():array("++(showUseVar.fst$ cqtuple rest)
                     ++"=>array("++(showUseVar.snd$ cqtuple rest)++")))",Set.empty)
     PHPAdd1{}
      -> line "addOneTo" addOneTo
     _ -> error ("cannot showCodeAndGetHeaders in Assignment of query "++show quer ++"(inexhaustive patterns in ShowCode.hs on line 100)")
    where
     line str set = ( str++"("++fst (assignment (cqfrom1 rest))++","++fst (assignment (cqfrom2 rest))++")"
                    , snd(assignment (cqfrom1 rest)) `Set.union` snd(assignment (cqfrom2 rest)) `Set.union` set)
   isElem = Set.singleton "function isElem($a,$b){ foreach($b as $c){ if($c==$a) return true;} return false;}"
   dbDoquer=Set.singleton "function DB_doquer_lookups($s){ $v=DB_doquer($s); $r=array(); foreach($v as $v2) addOneTo($r[$v2[0]],$v2[1]); return $r;}"
            `Set.union` addOneTo
   addOneTo=Set.singleton "function addOneTo(&$var,$val){if(!isset($var))$var=array();if(isset($val)) $var[]=$val;return $var;}"
