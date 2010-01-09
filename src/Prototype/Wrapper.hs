{-# OPTIONS_GHC -Wall #-}
 module Prototype.Wrapper (objectWrapper) where
   import Strings(chain)
   import Adl
   import Prototype.RelBinGenBasics(indentBlock,phpIdentifier,isOne,commentBlock,addToLast)
   import Data.Fspec
   import Version (versionbanner)

   objectWrapper :: Fspc -> ObjectDef -> String
   objectWrapper fSpec o
    = chain "\n" $
      [ "<?php // generated with "++versionbanner ]
      ++ commentBlock ["","  Interface V1.3.1","","","  Using interfaceDef",""] ++
      [ "  error_reporting(E_ALL); "
      , "  ini_set(\"display_errors\", 1);"
      , "  require \"interfaceDef.inc.php\";"
      , "  require \""++objectName++".inc.php\";"
      , "  require \"connectToDataBase.inc.php\";"
      , "  if(isset($_REQUEST['save'])) { // handle ajax save request (do not show the interface)"
      ] ++ (if isOne o then [] else [ "    $ID=@$_REQUEST['ID'];" ]) ++
      [ "    // we posted . characters, but something converts them to _ (HTTP 1.1 standard)"
      , "    $r=array();"
      , "    foreach($_REQUEST as $i=>$v){"
      , "      $r[join('.',explode('_',$i))]=$v; //convert _ back to ."
      , "    }"] ++
      indentBlock 4 (concat [phpList2Array 0 ("$"++phpIdentifier (name a)) (show n) a
                            | (a,n)<-zip (objats o) [(0::Integer)..]]
                    ) ++
      [ "    $"++objectId++"=new "++objectId++"(" ++ (if isOne o then [] else "$ID,") ++
        chain ", " ["$"++phpIdentifier (name a) | a<-objats o]++");"
      , "    if($"++objectId++"->save()!==false) die('ok:'."++selfref++
               (if isOne o then [] else ".'&" ++ objectId ++"='.urlencode($"++objectId++"->getId())")
               ++"); else die('Please fix errors!');"
      , "    exit(); // do not show the interface"
      , "  }"
      , "  $buttons=\"\";"] ++
      indentBlock 2
      ( if isOne o
        then [ "if(isset($_REQUEST['edit'])) $edit=true; else $edit=false;"
             , "$"++objectId++"=new "++objectId++"();"]
             ++ indentBlock 2 showObjectCode
             ++ [ "if(!$edit) $buttons.=ifaceButton("++selfref++".\"&edit=1\",\"Edit\");"
                , "else"
                , "  $buttons.=ifaceButton(\"JavaScript:save('\"."++selfref++".\"&save=1');"
                                                                                  ++ "\",\"Save\")"
                , "           .ifaceButton("++selfref++",\"Cancel\");"
                ]
        else [ "if(isset($_REQUEST['new'])) $new=true; else $new=false;"
             , "if(isset($_REQUEST['edit'])||$new) $edit=true; else $edit=false;"
             , "$del=isset($_REQUEST['del']);"
             , "if(isset($_REQUEST['"++objectId++"'])){"
             , "  if(!$del || !del"++objectId++"($_REQUEST['"++objectId++"']))" 
             , "    $"++objectId++" = read"++objectId++"($_REQUEST['"++objectId++"']);"
             , "  else $"++objectId++" = false; // delete was a succes!"
             , "} else if($new) $"++objectId++" = new "++objectId++"();"
             , "else $"++objectId++" = false;"
             , "if($"++objectId++"){"]
             ++ indentBlock 2 showObjectCode ++
             [ " if($del) echo \"<P><I>Delete failed</I></P>\";"
             , " if($edit){"
             , "   if($new) "
             , "     $buttons.=ifaceButton(\"JavaScript:save('\"."++selfref++".\"&save=1',"++
                              "document.forms[0].ID.value);\",\"Save\");"
             , "   else { "
             , "     $buttons.=ifaceButton(\"JavaScript:save('\"."++selfref++".\"&save=1',"++
                              "'\".urlencode($"++ objectId ++ "->getId()).\"');\",\"Save\");"
             , "     $buttons.=ifaceButton(" ++ selfref1 objectId ++ ",\"Cancel\");"
             , "   } "
             , "} else $buttons.=ifaceButton(" ++ selfref2 objectId "edit" ++ ",\"Edit\")"
             , "               .ifaceButton(" ++ selfref2 objectId "del" ++ ",\"Delete\");"
             , "}else{"
             , "  if($del){"
             , "    writeHead(\"<TITLE>Delete geslaagd</TITLE>\");"
             , "    echo 'The "++objectName++" is deleted';"
             , "  }else{  // deze pagina zou onbereikbaar moeten zijn"
             , "    writeHead(\"<TITLE>No "++objectName++" object selected - "
                    ++ appname ++" - ADL Prototype</TITLE>\");"
             , "    ?><i>No "++objectName++" object selected</i><?php "
             , "  }"
             , "  $buttons.=ifaceButton($_SERVER['PHP_SELF'].\"?new=1\",\"New\");"
             , "}"
             ]
      ) ++
      [ "  writeTail($buttons);"
      , "?>"
      ]
      where
        selfref2 objid act = "serviceref($_REQUEST['content'], array('"++objid++"'=>urlencode($"++objid++"->getId()),'"++act++"'=>1))"
        selfref1 objid = "serviceref($_REQUEST['content'], array('"++objid++"'=>urlencode($"++objid++"->getId()) ))"
        selfref = "serviceref($_REQUEST['content'])"
        phpList2Array :: Int->String->String->ObjectDef->[String]
        phpList2Array depth var rqvar a
         = if not (isUni (objctx a))
           then [ var++"=array();"
                , "for($i"++show depth++"=0;isset($r['"++rqvar++".'.$i"++show depth++"]);$i"
                       ++show depth++"++){"]
                ++ indentBlock 2 (phpList2ArrayUni (depth+1)
                                                   (var++"[$i"++show depth++"]")
                                                   (rqvar++".'.$i"++show depth++".'")
                                                   a
                                 ) ++
                [ "}"]
           else if not (isTot (objctx a))
                then ["if(@$r['"++rqvar++"']!=''){"]
                     ++ indentBlock 2 (phpList2ArrayUni depth var rqvar a) ++
                     ["}else "++var++"=null;"]
                else (phpList2ArrayUni depth var rqvar a)
        phpList2ArrayUni depth var rqvar a
         = addToLast ";" ([ var++" = "++head (phpList2ArrayVal var rqvar a)] ++
                          indentBlock (8 + length var) (tail (phpList2ArrayVal var rqvar a))
                         ) ++
           concat
           [ phpList2Array depth (var++"['"++name a'++"']") (rqvar++"."++show n) a'
           | (a',n)<-zip (objats a) [(0::Integer)..],not (isUni (objctx a'))]
        phpList2ArrayVal :: String->String->ObjectDef->[String]
        phpList2ArrayVal var rqvar a
         = if null (objats a) then ["@$r['"++rqvar++"']"]
           else [ "array( 'id' => @$r['"++rqvar'++"']"] ++ 
                [ ", '" ++ name a' ++ "' => "
                  ++ concat (phpList2ArrayVal var (rqvar++'.':show n) a')
                | (a',n)<-zip (objats a) [(0::Integer)..], isUni (objctx a')] ++
                [ ")"]
                 -- we gebruiken voor rqvar' liever iets waarvan het attribuut ingesteld wordt:
           where rqvar' = head ( [(rqvar++'.':show n)
                                 | (a',n)<-zip (objats a) [(0::Integer)..]
                                 , isUni (objctx a')
                                 , isIdent (objctx a')
                                 ] ++ [rqvar] )
        objectName      = name o
        appname         = name fSpec
        objectId        = phpIdentifier objectName
        isString object = not (isOne object) -- todo
        displaydirective obj = [(takeWhile (/='.') x,tail$dropWhile (/='.') x) 
                               | strs<-objstrs obj,('D':'I':'S':'P':'L':'A':'Y':'=':x)<-strs, elem '.' x]
        displaytbl obj = fst(head$displaydirective obj)
        displaycol obj = snd(head$displaydirective obj)
        showObjectCode
         = [ "writeHead(\"<TITLE>"++objectName++" - "++(appname)++" - ADL Prototype</TITLE>\""
           , "          .($edit?'<SCRIPT type=\"text/javascript\" src=\"edit.js\"></SCRIPT>':'<SCRIPT type=\"text/javascript\" src=\"navigate.js\"></SCRIPT>').\"\\n\" );"
           , "if($edit)"
           , "    echo '<FORM name=\"editForm\" action=\"'"
           ,"          .$_SERVER['PHP_SELF'].'\" method=\"POST\" class=\"Edit\">';"]++
           ( if isString o
             then ["if($edit && $"++objectId++"->isNew())"
                  ,"     echo '<P><INPUT TYPE=\"TEXT\" NAME=\"ID\" VALUE=\"'.addslashes($"++objectId++"->getId()).'\" /></P>';"
                  ,"else echo '<H1>'."++
                       (if null (displaydirective o) then ("$"++objectId++"->getId()") 
                        else "display('"++displaytbl o++"','"++displaycol o++"',$"++objectId++"->getId())")
                       ++".'</H1>';"
                  ,"?>"
                  ]
             else ["?><H1>"++objectName++"</H1>"]
           )
           ++ (concat [attributeWrapper (0::Integer) (show n) (if(length(objats o)>1) then "_"++phpIdentifier (name a) else "") a | (a,n)<-zip (objats o) [(0::Integer)..]])
           ++ ["<?php"
              ,"if($edit) echo '</FORM>';"
              ]
        showBlockJS cls att
         = [ "function UI"++cls++"(id){"
           , "  return " ++ head attCode'] ++ (map ((++) "       + ") (tail attCode')) ++
           [ "        ;", "}"]
           where
             attCode' = map (\x->"'"++x++"'") (attCode "'+id+'." cls att)
             attCode strt c at = ["<DIV>"++(name a)++": "++(specifics c (strt ++ show n) a)++"</DIV>"
                                 | (n,a)<-zip [(0::Integer)..] (objats at)]
             specifics c n a = if isUni (objctx a)
                                 then if isTot (objctx a)
                                      then "<SPAN CLASS=\"item UI"++acls c a++"\" ID=\""++n
                                           ++"\">" ++ concat (attCode n (acls c a) a) ++"</SPAN>"
                                      else "<DIV CLASS=\"new UI"++acls c a++"\" ID=\""
                                           ++n++"\"><I>Nothing</I></DIV>"
                                 else "<UL><LI CLASS=\"new UI"++acls c a
                                      ++"\" ID=\""++n++"\">new "++(name a)++"</LI></UL>"
             acls c a = c++"_"++(phpIdentifier (name a))
        attributeWrapper depth path cls att
         = if elem "PICTURE" [x|xs<-objstrs att,x<-xs] 
           then   
           [ "<?php"
           , "      $"++ phpIdentifier (name att) ++" = $" ++ objectId ++ "->get_" ++ phpIdentifier (name att)++"();"
           ] ++ indentBlock 6 embedimage ++
           [ "    ?> "]
           else
           [ "<DIV class=\"Floater "++(name att)++"\">"
           , "  <DIV class=\"FloaterHeader\">"++(name att)++"</DIV>"
           , "  <DIV class=\"FloaterContent\"><?php"
           , "      $"++ phpIdentifier (name att) ++" = $" ++ objectId ++ "->get_" ++ phpIdentifier (name att)++"();"
           ] ++ indentBlock 6 content ++
           [ "    ?> "
           , "  </DIV>"
           , "</DIV>"] ++
           if null newBlocks then [] else
           [ "<?php if($edit){ ?>"
           , "<SCRIPT type=\"text/javascript\">"
           , "  // code for editing blocks in "++(name att)
           ]
           ++ indentBlock 2 (concat [showBlockJS c a | (c,a)<-newBlocks]) ++
           [ "</SCRIPT>"
           , "<?php } ?>"]
           where 
            embedimage
              = ["foreach("++var++" as $i"++show depth++"=>"++atnm ++"){"
                , "  echo '<IMG src=\"'."++atnm++".'\"/>';"
                , "}"]
                where
                var =  ("$"++phpIdentifier (name att))
                atnm = "$v"++show depth
            (content,newBlocks)
              = (attContent ("$"++phpIdentifier (name att)) depth path cls att)
        -- attHeading shows a heading and its value
        attHeading var depth path cls att | objats att==[]
         = (["echo '"++name att++": ';"] ++ content,newBlocks)
           where (content,newBlocks) = attContent var depth path cls att
        attHeading var depth path cls att | otherwise
         = ([ "?> "
           , "<DIV class =\"Holder\"><DIV class=\"HolderHeader\">"++(name att)++"</DIV>"
           , "  <DIV class=\"HolderContent\" name=\""++(name att)++"\"><?php"
           ] ++ indentBlock 6 content ++
           [ "    ?> "
           , "  </DIV>"
           , "</DIV>"
           , "<?php"
           ],newBlocks)
           where (content,newBlocks) = attContent var depth path cls att
        -- attContent shows a list of values, using uniAtt if it is only one
        attContent var depth path cls att | not (isUni (objctx att))
         = ([ "echo '"
           , "<UL>';"
           , "foreach("++var++" as $i"++show depth++"=>"++idvar ++"){"
           , "  "++atnm ++"="++(if null (displaydirective att) then idvar 
                          else "display('"++displaytbl att++"','"++displaycol att++"',"++idvar++")") ++ ";"
           , "  echo '"
           , "  <LI CLASS=\"item UI"++cls++"\" ID=\""++(path ++".'.$i"++show depth++".'")++"\">';"]
           ++ indentBlock 4 content ++
           [ "  echo '</LI>';"
           , "}"
           , "if($edit) echo '"
           , "  <LI CLASS=\"new UI"++cls++ "\" ID=\""
                              ++(path ++".'.count("++var++").'")++"\">new "++name att++"</LI>';"
           , "echo '"
           , "</UL>';"
           ], (if null (objats att) then [] else [(cls,att)])++ newBlocks)
           where
            (content,newBlocks)
             = uniAtt atnm idvar (depth+1)
                      (path ++".'.$i"++show depth++".'") cls att
            atnm = if "$"++phpIdentifier (name att)==var then "$v"++show depth else "$"++phpIdentifier (name att)
            idvar = if "$"++phpIdentifier (name att)==var then "$idv"++show depth else "$id"++phpIdentifier (name att)
        attContent  var depth path cls att | objats att==[]
         = if isTot (objctx att)
           then ([ "echo '<SPAN CLASS=\"item UI"++cls++"\" ID=\""++path++"\">';" 
                 , "  "++dvar var ++"="++(if null (displaydirective att) then var 
                          else "display('"++displaytbl att++"','"++displaycol att++"',"++var++")") ++ ";"]
                ++ content ++ [ "echo '</SPAN>';" ],newBlocks)
           else ([ "if (isset("++var++")){"
                 , "  "++dvar var ++"="++(if null (displaydirective att) then var 
                          else "display('"++displaytbl att++"','"++displaycol att++"',"++var++")") ++ ";"
                 , "  echo '<DIV CLASS=\"item UI"++cls++"\" ID=\""++path++"\">';"
                 , "  echo '</DIV>';"] ++ indentBlock 2 content ++
                 [ "} else echo '<DIV CLASS=\"new UI"++cls++"\" ID=\""++path++"\"><I>Nothing</I></DIV>';"
                 ],newBlocks)
           where (content,newBlocks) = uniAtt (dvar var) var depth path cls att
         --       spanordiv = if isTot (objctx att) then "SPAN" else "DIV"
                 dvar var'@('$':x) = if null (displaydirective att) then var' else ('$':("display"++x))
                 dvar x = x
        attContent  var depth path cls att | (isTot(objctx att))
         = ([ "echo '<DIV CLASS=\"UI"++cls++"\" ID=\""++path++"\">';" ]
           ++ indentBlock 2 content ++
           [ "echo '</DIV>';" ],newBlocks)
           where
            (content,newBlocks) = uniAtt (var) var depth path cls att
        attContent  var depth path cls att | otherwise
         = ([ "if(isset("++var++")){"
            , "  echo '<DIV CLASS=\"item UI"++cls++"\" ID=\""++path++"\">';"]
            ++ indentBlock 4 content ++
            [ "  echo '</DIV>';"
            , "}else{"
            , "  echo '<DIV CLASS=\"new UI"++cls++"\" ID=\""++path++"\"><I>Nothing</I></DIV>';"
            , "}"]
           ,(if tot then [] else [(cls,att)]) ++ newBlocks
           )
           where
            (content,newBlocks) = uniAtt (var) var depth path cls att
            tot = (isTot(objctx att))
        gotoPages :: ObjectDef->String->[(String,String)]
        gotoPages att idvar
          = [ ("'.serviceref('"++name serv++"', array('"++(phpIdentifier$name serv)++"'=>urlencode("++idvar++"))).'"
              ,name serv)
            | serv<-(serviceS fSpec)
            , target (objctx serv) == target (objctx att)
            ]
        gotoDiv gotoP path
         = [ "echo '<DIV class=\"Goto\" id=\"GoTo"++path++"\"><UL>';"] ++
           [ "echo '<LI><A HREF=\""++link++"\">"++txt++"</A></LI>';"
           | (link,txt) <- gotoP] ++
           [ "echo '</UL></DIV>';" ]
        uniAtt var idvar _ path _ att | null (objats att)
         = (if not (isTot (objctx att)) && isUni (objctx att)
           then [ "if(isset("++var++")){" ] ++ indentBlock 2 content ++ ["}"]
           else content,[])
           where
            content=if null gotoP || isIdent (ctx att) then ["echo "++echobit++";"]
                    else if length gotoP == 1
                         then ["if(!$edit) echo '"
                              ,"<A HREF=\""++(fst$head gotoP)++"\">'."++echobit++".'</A>';"
                              ,"else echo "++echobit++";"]
                         else ["if(!$edit){"
                              ,"  echo '"
                              ,"<A class=\"GotoLink\" id=\"To"++path++"\">';"
                              ,"  echo "++echobit++".'</A>';"]
                              ++ indentBlock 2 (gotoDiv gotoP path) ++
                              [ "} else echo "++echobit++";" ]
            echobit= "htmlspecialchars("++var++")"
            gotoP = gotoPages att idvar
        uniAtt var idvar depth path cls att
         = ((if null gotoP then []
             else if length gotoP == 1
                  then [ "if(!$edit){"
                       , "  echo '"
                       , "<A HREF=\""++(fst$head gotoP)++"\">';"
                       , "  echo '<DIV class=\"GotoArrow\">&rarr;</DIV></A>';"
                       , "}" ]
                  else [ "if(!$edit){"
                       , "  echo '"
                       , "<DIV class=\"GotoArrow\" id=\"To"++path++"\">&rArr;</DIV>';"]
                       ++ indentBlock 2 (gotoDiv gotoP path) ++
                       [ "}" ]
            )++
            ["echo '"
            ,"<DIV>';"]
            ++ chain ["echo '</DIV>"
                     ,"<DIV>';"] content
            ++ ["echo '"
               ,"</DIV>';"
               ,"if($edit) echo '"
               ,"<INPUT TYPE=\"hidden\" name=\""++path++".ID\" VALUE=\"'."++var++"['id'].'\" />';"
               ]
               ,newBlocks)
           where
            gotoP = gotoPages att (idvar ++ "['id']")
            newBlocks = concat $ map snd stuff
            content = map fst stuff
            stuff
             = [ (indentBlock 2 c, b)
               | (a,n)<-zip (objats att) [(0::Integer)..]
               , (c,b)<-[attHeading (var++"['"++name a++"']") depth (path++"."++show n)
                                    (cls ++ if length(objats att) > 1
                                            then (if null cls then "" else "_")
                                                 ++ phpIdentifier (name a)
                                            else "") a
                        ]]
