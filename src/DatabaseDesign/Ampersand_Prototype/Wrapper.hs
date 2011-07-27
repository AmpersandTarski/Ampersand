{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.Wrapper (objectWrapper) where
import Data.List
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics(indentBlock,phpIdentifier,commentBlock,addToLast)
import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL(isOne)
import DatabaseDesign.Ampersand_Prototype.Version 

--ifcs is needed to determine whether some instance of a concept has a wrapper web page (*.php) to display it i.e. does it become a HTTP-link
objectWrapper :: Fspc -> [Interface] ->  Interface -> Options -> String
objectWrapper fSpec ifcs ifc flags
 = intercalate "\n" $
   [ "<?php // generated with "++ampersandPrototypeVersionBanner ]
  ++
   commentBlock ["","  Interface V1.3.1","","","  Using interfaceDef",""]
  ++
   [ "  error_reporting(E_ALL); "
   , "  ini_set(\"display_errors\", 1);"
   , "  require \"interfaceDef.inc.php\";"
   , "  require \""++objectName++".inc.php\";"
   , "  require \"connectToDataBase.inc.php\";"
   ]
  ++ --BEGIN: handle save request
   [ "  if(isset($_REQUEST['save'])) { // handle ajax save request (do not show the interface)"]
  ++
   [ "    // we posted . characters, but something converts them to _ (HTTP 1.1 standard)"
   , "    $r=array();"
   , "    foreach($_REQUEST as $i=>$v){"
   , "      $r[join('.',explode('_',$i))]=$v; //convert _ back to ."
   , "    }"
   ]
  ++ --(see phpList2Array below)
   indentBlock 4 (concat [phpList2Array 0 ("$"++phpIdentifier (name a)) (show n) a | (a,n)<-zip (objats o) [(0::Integer)..]])
  ++
   ( if isOne o
     then [ "    $"++objectId++"=new "++objectId++"(" ++ intercalate ", " ["$"++phpIdentifier (name a) | a<-objats o]++",False);"
          , "    if($"++objectId++"->save()!==false) die('ok:'."++selfref++");"
          ] 
     else [ "    $"++objectId++"=new "++objectId++"(@$_REQUEST['ID']" ++ [','|not(null (objats o))]
                                  ++ intercalate ", " ["$"++phpIdentifier (name a) | a<-objats o]++",False);"
          , "    if($"++objectId++"->save()!==false) die('ok:'."++selfref++".'&" ++ objectId ++"='.urlencode($"++objectId++"->getId())"++");"
          ] 
   )
  ++
   [ "    else die('Please fix errors!');"
   , "    exit(); // do not show the interface"
   , "  }" 
   ] --END:handle save request
  ++ 
   [ "  $buttons=\"\";" ]
  ++ --BEGIN:editing + showObjectCode
   indentBlock 2
   ( if isOne o
     then [ "if(isset($_REQUEST['edit'])) $edit=true; else $edit=false;" ]
         ++
          [ "$"++objectId++"=new "++objectId++"();" ]
         ++ 
          [ "if(!$edit) "++
                if visibleedit 
                then "$buttons.=ifaceButton("++selfref++".\"&edit=1\",\"Edit\");"
                else "$buttons=$buttons;"
          , "else"
          , "  $buttons.=ifaceButton(\"JavaScript:save('\"."++selfref++".\"&save=1');\",\"Save\")"
          , "           .ifaceButton("++selfref++",\"Cancel\");"
          ]
         ++ indentBlock 2 showObjectCode --(see showObjectCode below)
     else [ "if(isset($_REQUEST['new'])) $new=true; else $new=false;"
          , "if(isset($_REQUEST['edit'])||$new) $edit=true; else $edit=false;"
          , "$del=isset($_REQUEST['del']);"
          , "if(isset($_REQUEST['"++objectId++"'])){"
          , "  if(!$del || !del"++objectId++"($_REQUEST['"++objectId++"']))" 
          , "    $"++objectId++" = read"++objectId++"($_REQUEST['"++objectId++"']);"
          , "  else $"++objectId++" = false; // delete was a succes!"
          , "} else if($new) $"++objectId++" = new "++objectId++"();"
          , "else $"++objectId++" = false;"
          ]
         ++
          [ "if($"++objectId++"){" ]
         ++
          [ " if($del) echo \"<P><I>Delete failed</I></P>\";"
          , " if($edit){"
          , "   if($new) "
          , "     $buttons.=ifaceButton(\"JavaScript:save('\"."++selfref++".\"&save=1', document.forms[0].ID.value);\",\"Save\");"
          , "   else { "
          , "     $buttons.=ifaceButton(\"JavaScript:save('\"."++selfref++".\"&save=1','\".urlencode($"++ objectId ++ "->getId()).\"');\",\"Save\");"
          , "     $buttons.=ifaceButton(" ++ selfref1 objectId ++ ",\"Cancel\");"
          , "   } "
          , "} else {"
          , if visibleedit
            then "        $buttons.=ifaceButton(" ++ selfref2 objectId "edit" ++ ",\"Edit\");"
            else "        $buttons=$buttons;"
          , if visibledel
            then "        $buttons.=ifaceButton(" ++ selfref2 objectId "del" ++ ",\"Delete\");"
            else "        $buttons=$buttons;"
          , "       }"]
         ++ indentBlock 2 showObjectCode --(see showObjectCode below)
         ++
          [ "}else{"
          , "  if($del){"
          , "    writeHead(\"<TITLE>Delete geslaagd</TITLE>\");"
          , "    echo 'The "++objectName++" is deleted';"
          , "  }else{  // deze pagina zou onbereikbaar moeten zijn"
          , "    writeHead(\"<TITLE>No "++objectName++" object selected - " ++ appname ++" - Ampersand Prototype</TITLE>\");"
          , "    ?><i>No "++objectName++" object selected</i><?php "
          , "  }"
          , "}"
          ]
   )
  ++
   [ "  writeTail();"
   , "?>"
   ]
   where
   o               = ifcObj ifc
   objectName      = name o
   objectId        = phpIdentifier objectName
   appname         = name fSpec
   editable | theme flags==StudentTheme =  [r|("Student",r)<-mayEdit fSpec]
            | otherwise = map makeRelation (declarations fSpec) ++map I (concs fSpec)
   visibleedit = foldr (||) visiblenew [mayedit x editable|x<-allobjctx o]
   visibledel = False --del() not implemented yet -- visiblenew --if you may add you may delete 
   visiblenew = mayadd (target(objctx o)) editable
   allobjctx obj = (objctx obj):(concat (map allobjctx (objats obj)))
   showObjectCode --display some concept instance in read or edit mode by definition of INTERFACE
    = [ "writeHead(\"<TITLE>"++objectName++" - "++(appname)++" - Ampersand Prototype</TITLE>\""
      , "          .($edit?'<SCRIPT type=\"text/javascript\" src=\"js/edit.js\"></SCRIPT>':'').'<SCRIPT type=\"text/javascript\""
                ++ " src=\"js/navigate.js\"></SCRIPT>'.\"\\n\", $buttons );"
      , "if($edit)"
      , "    echo '<FORM name=\"editForm\" action=\"'.$_SERVER['PHP_SELF'].'\" method=\"POST\" class=\"Edit\">';"
      ]
     ++ --BEGIN: display/edit the identifier of some concept instance
      ( if not (isOne o)
        then ["if($edit && $"++objectId++"->isNew())"
             ,if autoid flags --if not autoid then the user has to come up with an identifier for the new instance
              then "     echo '<H1>New instance of "++objectName++"</H1>';"--TODO maybe I should generate an ID here and put it in a hidden INPUT NAME=ID
              else "     echo '<P><INPUT TYPE=\"TEXT\" NAME=\"ID\" VALUE=\"'.addslashes($"++objectId++"->getId()).'\" /></P>';"
             ,"else echo '<H1>'."++
                  (if null (displaydirective o) 
                   then ("$"++objectId++"->getId()") 
                   else "display('"++displaytbl o++"','"++displaycol o++"',$"++objectId++"->getId())")
                  ++".'</H1>';"
             ,"?>"
             ]
        else ["?><H1>"++objectName++"</H1>"] --the context element is a constant, it is nicer to display the ifclabel (objectName)
      ) --END: display/edit the identifier of some concept instance
     ++ concat [attributeWrapper ifcs editable objectId (show n) (length(objats o)>1) a | (a,n)<-zip (objats o) [(0::Integer)..]]
     ++  --(see attributeWrapper below)
      ["<?php"
      ,"if($edit) echo '</FORM>';"
      ]

-----------------------------------------
--some small functions
-----------------------------------------
selfref2::String->String->String
selfref2 objid act = "interfaceref($_REQUEST['content'],false,false, array('"++objid++"'=>urlencode($"++objid++"->getId()),'"++act++"'=>1))"
selfref1::String->String
selfref1 objid = "interfaceref($_REQUEST['content'],false,false, array('"++objid++"'=>urlencode($"++objid++"->getId()) ))"
selfref::String
selfref = "interfaceref($_REQUEST['content'])"
displaydirective::ObjectDef->[(String,String)]
displaydirective obj = [(takeWhile (/='.') x,tail$dropWhile (/='.') x) 
                       | strs<-objstrs obj,('D':'I':'S':'P':'L':'A':'Y':'=':x)<-strs, elem '.' x]
displaytbl::ObjectDef->String
displaytbl obj = fst(head$displaydirective obj)
displaycol::ObjectDef->String
displaycol obj = snd(head$displaydirective obj)
--use novalue to display an item which does not exist
novalue::String
novalue = "<I>Nothing</I>"
--class "item UI*" and "new UI*" are editable (see edit.js)
itemUI :: [Relation Concept] -> Expression(Relation Concept) -> String
itemUI editable item
  | mayedit item editable = "item UI"
  | otherwise = "itemshow UI"
newUI :: [Relation Concept] -> Expression(Relation Concept) -> String
newUI editable item
  | mayedit item editable = "new UI"
  | otherwise = "itemshow UI"
mayedit :: Expression(Relation Concept) -> [Relation Concept] -> Bool
mayedit item editable = let rexprs=[Tm r|r<-editable] in elem item (rexprs++map flp rexprs)
mayadd :: Concept -> [Relation Concept] -> Bool
mayadd cpt editable = (not.null) [()|r<-editable,isIdent r||isTrue r,target r==cpt] 

-----------------------------------------
--display/edit the instances related to the identifier of some concept instance (objectId) by definition of INTERFACE (att0)
--ifcs is nodig voor GoToPages
-- "$" ++ objectId is de instantie van de class die je op het scherm ziet
--path0 is een op atts gezipt nummertje. Er wordt een wrapper gemaakt voor iedere [wrapper (show n) att0|(att0,n)<-atts o]
--siblingatt0s bepaalt of er meer dan 1 (wrapper att0) is. Deze info is nodig om te bepalen of CLASS = '.. UI of UI_*'.
--att0 is de huidige subinterface
attributeWrapper::[Interface]->[Relation Concept]->String->String->Bool->ObjectDef->[String]
attributeWrapper ifcs editable objectId path0 siblingatt0s att0
 = let 
   cls0 | siblingatt0s = "_"++phpIdentifier (name att0) 
        | otherwise    = ""
   content = attContent ("$"++phpIdentifier (name att0)) (0::Integer) path0 cls0 att0
   newBlocks = attEdit ("$"++phpIdentifier (name att0)) (0::Integer) path0 cls0 att0
   in
   --BEGIN : content (in read or edit mode)
   --By default javascript controls read and edit mode layout for "item UI" and "new UI" (see edit.js for edit mode)
   --other items are the same in read and edit mode
   --navigate.js handles Goto* classes
   if elem "PICTURE" [x|xs<-objstrs att0,x<-xs] 
   --TODO-> replace by checking (target att0)==Cpt "Picture" && Picture ISA Datatype
   --The meaning of and the concept name "Datatype" is claimed by Ampersand.
   --The meaning of and the concept name "String" is claimed by Ampersand. (GEN String ISA Datatype)
   --other specific datatypes have to be declared to get the Ampersand meaning e.g. GEN Picture ISA Datatype
   --the Ampersand meaning of Picture is that its value (value::Picture->String[INJ].) is an URL string
   --we could define GEN URL ISA String and declare rules for URL's
   --TODO -> Pictures are presented the same in read and edit mode (not editable) => develop class(es) for pictures
   then --in Atlas.adl GEN Picture ISA Datatype is needed instead of the current default GEN String ISA Datatype
   [ "<?php"
   , "      $"++ phpIdentifier (name att0) ++" = $" ++ objectId ++ "->get_" ++ phpIdentifier (name att0)++"();"
   ]
   ++ indentBlock 6 (embedimage att0 0) --(see embedimage below)
   ++
   [ "    ?> "]
   else
   [ "<DIV class=\"Floater "++(name att0)++"\">"
   , "  <DIV class=\"FloaterHeader\">"++(name att0)++"</DIV>"
   , "  <DIV class=\"FloaterContent\"><?php"
   , "      $"++ phpIdentifier (name att0) ++" = $" ++ objectId ++ "->get_" ++ phpIdentifier (name att0)++"();" --read instance from DB
   ] 
   ++ indentBlock 6 content --(see attContent below)
   ++
   [ "    ?> "
   , "  </DIV>"
   , "</DIV>"
   ] --END: content
   ++ --BEGIN: edit blocks (see attEdit below)
   --REQUEST FOR COMMENT: what do the edit blocks do at runtime?
   if null newBlocks then [] 
   else
   [ "<?php if($edit){ ?>"
   , "<SCRIPT type=\"text/javascript\">"
   , "  // code for editing blocks in "++(name att0)
   ]
   ++ indentBlock 2 (concat [showBlockJS c editable a | (c,a)<-newBlocks])
   ++
   [ "</SCRIPT>"
   , "<?php } ?>"
   ] --END: edit blocks
   where    
   -----------------------------------------
   --CONTENT functions
   -----------------------------------------
   --the content for an att0 has already been read in a php class instance for this interface
   -- recall: $"++ phpIdentifier (name att0) ++" = $" ++ objectId ++ "->get_" ++ phpIdentifier (name att0)++"();"
   --content enters at attContent with 
   --   att=att0
   --   var=("$"++phpIdentifier (name att0))
   --   depth=(0::Integer),path=path0,cls=cls0
   --attContent makes a suitable frame based on the multiplicity of (objctx att)
   --depth increases with 1 iff not(UNI), this should sync with phpList2Array (saving request) and attEdit (javascript functions) for synced paths
   --uniAtt prints the values as links in read mode and as values in edit mode
   --if there are objats, then the instances on this level are printed as headers with links
   --through attHeaders the recursion of attContent is made with
   --   att'= a
   --   var'=(var++"['"++name a++"']")
   --   path'=(path++"."++show n)
   -----------
   --gotoPages returns the list of suitable interface links for an (objctxatt) in edit or read mode and a name for it
   gotoPages :: ObjectDef->String->[(String,String)]
   gotoPages att idvar 
     = [ ("'.interfaceref('"++name ifc++"',false,$edit, array('"++(phpIdentifier$name ifc)++"'=>urlencode("++idvar++"))).'"
         ,name ifc)
       | ifc<-ifcs
       , target (objctx (ifcObj ifc)) == target (objctx att)
       ]
   --gotoPagesNew like gotoPages only in new mode
   gotoPagesNew att
     = [ ("'.interfaceref('"++name ifc++"',$edit).'"
         ,"new "++ name ifc)
       | ifc<-ifcs
       , target (objctx (ifcObj ifc)) == target (objctx att)
       ]
   --In case there is more than one gotoPage, gotoDiv generates suitable code for a gotoPage
   gotoDiv gotoP path
    = [ "echo '<DIV class=\"Goto\" id=\"GoTo"++path++"\"><UL>';"] ++
      [ "echo '<LI><A HREF=\""++link++"\">"++txt++"</A></LI>';"
      | (link,txt) <- gotoP] ++
      [ "echo '</UL></DIV>';" ]
   ----------------
   -- attContent shows a list of values, using uniAtt if it is only one
   attContent var depth path cls att 
    | not (isUni (objctx att)) --(not(UNI) with or without objats) 
      = let
        content = uniAtt atnm idvar (depth+1) (path ++".'.$i"++show depth++".'") cls att
        atnm = if "$"++phpIdentifier (name att)==var then "$v"++show depth else "$"++phpIdentifier (name att)
        idvar = if "$"++phpIdentifier (name att)==var then "$idv"++show depth else "$id"++phpIdentifier (name att)
        gotoP = gotoPagesNew att
        in
        [ "echo '"
        , "<UL>';"
        , "foreach("++var++" as $i"++show depth++"=>"++idvar ++"){"
        , --atnm is set to I(idvar) or value(idvar), where value is the name of the display relation
          "  "++atnm ++"="++(if null (displaydirective att) then idvar
                       else (if null(objats att)
                             then "display('"++displaytbl att++"','"++displaycol att++"',"++idvar++")"
                             else idvar)) ++ ";" 
        , "  echo '"
        , "  <LI CLASS=\""++itemUI editable (objctx att)++cls++"\" ID=\""++(path ++".'.$i"++show depth++".'")++"\">';"
        , if null(objats att) || null(displaydirective att) 
          then [] 
          else "  echo display('"++displaytbl att++"','"++displaycol att++"',"++idvar++"['id']);"
        ]
        ++ indentBlock 4 content ++
        [ "  echo '</LI>';"
        , "}"
        ]
        ++ 
        --BEGIN:add item to editable item list
        --TODO new UI should become a dropdown to create a new relation instance, including <new concept instance> which are links (gotoP)
        if not(mayedit (objctx att) editable) then [] else [ "if($edit) {"]
        ++
        [ "  echo '<LI CLASS=\"new UI"++cls++ "\" ID=\""++(path ++".'.count("++var++").'")
                   ++"\">vul een bestaand(e) "++(name.target.objctx) att++" in</LI>';"| not(isTrue(objctx att) || isIdent(objctx att))]
        ++
        (if not(null gotoP) && mayadd (target(objctx att)) editable 
         --if there is a INTERFACE for the target concept, and you may create new elements of that concept
         then 
         [ "  echo '<LI CLASS=\"newlink UI"++cls++ "\" ID=\""++(path ++".'.(count("++var++")+1).'")++"\">';"]
         ++ (if length gotoP==1 
             then [ "  echo '<A HREF=\""++(fst$head gotoP)++"\">maak nieuw(e) "++(name.target.objctx) att++"</A>';"]
             else [ "  echo '<A class=\"GotoLink\" id=\"To"++path++"\">maak nieuw(e) "++(name.target.objctx) att++"</A>';"]
                  ++ indentBlock 2 (gotoDiv gotoP path)
            )
         ++
         [ "  echo '</LI>';" ]
         else []
        )
        ++ [ "}" ]
        --END:add item to editable item list
        ++
        [ "echo '"
        , "</UL>';"
        ]
--    | objats att==[] --attContent (UNI without objats)
--      = let
--        content = uniAtt (dvar var) var depth path cls att
    --       spanordiv = if isTot (objctx att) then "SPAN" else "DIV"
--        dvar var'@('$':x) = if null (displaydirective att) then var' else ('$':("display"++x))
--        dvar x = x
--        val | null (displaydirective att) = var
--            | otherwise = "display('"++displaytbl att++"','"++displaycol att++"',"++var++")"
--        in
--        if isTot (objctx att) --waarom is TOT->SPAN en UNI->DIV?
--        then  [ "echo '<SPAN CLASS=\"item UI"++cls++"\" ID=\""++path++"\">';" 
--              , "  "++dvar var ++"="++ val ++ ";"]
--             ++ content 
--             ++ [ "echo '</SPAN>';" ]
--        else  [ "if (isset("++var++")){" --in case of TOT, $var must be set except in case of new mode
--              , "  "++dvar var ++"="++val ++ ";"
--              , "  echo '<DIV CLASS=\"item UI"++cls++"\" ID=\""++path++"\">';"
--              , "  echo '</DIV>';"]
--             ++ indentBlock 2 content ++
--              [ "} else echo '<DIV CLASS=\"new UI"++cls++"\" ID=\""++path++"\">"++novalue++"</DIV>';" ]
--    | (isTot(objctx att)) --attContent (UNI & TOT with objats)
--      = let
--        content = uniAtt (var) var depth path cls att
--        in --the relation instance exists or is pretended(!) to exist in case of new mode => item UI
--           --TODO -> when dropdown is implemented a <new instance> link exists in the dropdown
--           --        untill then new instances have to be created in a separate transaction (only picking existing)
--        [ "//PICK an existing item"++path++". Creating instances is not/should not be possible."
--        , "echo '<DIV CLASS=\"item UI"++cls++"\" ID=\""++path++"\">';" ] --TODO TEST: assumed that CLASS=UI should be CLASS=item UI
--        ++ indentBlock 2 content ++
--        [ "echo '</DIV>';" ]
    | otherwise --attContent (UNI with or without objats)
      = let
        content = uniAtt (var) var depth path cls att
        in
        --TODO -> when dropdown is implemented a <new instance> link exists in the dropdown
        --        untill then new instances have to be created in a separate transaction (only picking existing)
        [ "//PICK an existing item"++path++". Creating instances should at most be possible for simple Concepts."
        , "if(isset("++var++")){" --in case of TOT (+UNI), $var must be set.
                                  --TODO: in new mode it is implemented as $var='', which is wrong, because at save '' will become an instance of the target concept
           --the relation instance exists => item UI
        , "  echo '<DIV CLASS=\""++itemUI editable (objctx att)++cls++"\" ID=\""++path++"\">';"
        , "}else{"
          --no relation instance exists => new UI
        , "  echo '<DIV CLASS=\""++newUI editable (objctx att)++cls++"\" ID=\""++path++"\">';"
        , "}"]
        ++ indentBlock 4 content --uniAtt returns novalue if $var is not set or $var==''
        ++ [ "echo '</DIV>';"]
   ----------------end: attContent
   uniAtt var idvar depth path cls att
    | null (objats att)
      = let
        content=if null gotoP || isIdent (ctx att) then ["echo "++echobit++";"]
                else if length gotoP == 1
                     then ["if(!$edit) echo '"
                          ,if null (urlstrs att)
                           then "<A HREF=\""++(fst$head gotoP)++"\">'."++echobit++".'</A>';"
                           else "<A HREF=\""++head(urlstrs att)++"\">'."++echobit++".'</A>';"
                          ,"else echo "++echobit++";"]
                     else ["if(!$edit){"
                          ,"  echo '"
                          ,"<A class=\"GotoLink\" id=\"To"++path++"\">';"
                          ,"  echo "++echobit++".'</A>';"]
                          ++ indentBlock 2 (gotoDiv gotoP path) ++
                          [ "} else echo "++echobit++";" ]
        echobit= "htmlspecialchars("++var++")"
        gotoP = gotoPages att idvar
        in
        if isUni (objctx att) --TODO: how do you prevent the novalue to be saved?
        then [ "if(isset("++var++") && "++var++"!=''){" ] ++ indentBlock 2 content ++ ["} else {echo '"++novalue++"';}"]
        else [ "if("++var++"==''){echo '"++novalue++"';}", "else{"] ++ content ++ ["}"]
    | otherwise --uniAtt
      = let
        gotoP = gotoPages att (idvar ++ "['id']")
        echobit= "htmlspecialchars("++idvar ++ "['id'])"
        content 
         = [ indentBlock 2 c
           | (a,n)<-zip (objats att) [(0::Integer)..]
           , c<-[attHeading (var++"['"++name a++"']") depth (path++"."++show n)
                                (cls ++ if length(objats att) > 1
                                        then (if null cls then "" else "_")
                                             ++ phpIdentifier (name a)
                                        else "") a]]
        in
        (if null gotoP then []
         else if length gotoP == 1
              then ["if(!$edit) echo '"
                   ,if null (urlstrs att)
                    then"<A HREF=\""++(fst$head gotoP)++"\">'."++echobit++".'</A>';"
                    else"<A HREF=\""++head (urlstrs att)++"\">'."++echobit++".'</A>';"
                   ,"else echo '<DIV CLASS=\""++itemUI editable (objctx att)++cls++"\" ID=\""++path++"\">'."++echobit++".'</DIV>';"]
              else ["if(!$edit){"
                   ,"  echo '"
                   ,"<A class=\"GotoLink\" id=\"To"++path++"\">';"
                   ,"  echo "++echobit++".'</A>';"]
                   ++ indentBlock 2 (gotoDiv gotoP path) ++
                   [ "} else echo '<DIV CLASS=\""++itemUI editable (objctx att)++cls++"\" ID=\""++path++"\">'."++echobit++".'</DIV>';" ]
              --if length gotoP == 1
          --    then [ "if(!$edit){"
            --       , "  echo '"
              --     , "<A HREF=\""++(fst$head gotoP)++"\">';"
                   --, "  echo '<DIV class=\"GotoArrow\">&rarr;</DIV></A>';"
                --   , "  echo 'htmlspecialchars("++var++")</A>';"
                  -- , "}" ]
--              else [ "if(!$edit){"
  --                 , "  echo '"
    --               , "<DIV class=\"GotoArrow\" id=\"To"++path++"\">&rArr;</DIV>';"]
      --             ++ indentBlock 2 (gotoDiv gotoP path) ++
        --           [ "}" ]
        )++
        ["echo '"
        ,"<DIV>';"]
        ++ intercalate ["echo '</DIV>"
                       ,"<DIV>';"] content
        ++ ["echo '"
           ,"</DIV>';"
           ,"if($edit) echo '"
           ,"<INPUT TYPE=\"hidden\" name=\""++path++".ID\" VALUE=\"'."++var++"['id'].'\" />';"
           ]
   -----------end: uniAtt
   -- attHeading shows a heading and its value
   attHeading var depth path cls att 
    | objats att==[]
      = ["echo '"++name att++": ';"] ++ content
    | otherwise
      = [ "?> "
        , "<DIV class =\"Holder\"><DIV class=\"HolderHeader\">"++(name att)++"</DIV>"
        , "  <DIV class=\"HolderContent\" name=\""++(name att)++"\"><?php"
        ] ++ indentBlock 6 content ++
        [ "    ?> "
        , "  </DIV>"
        , "</DIV>"
        , "<?php"
        ]
      where content = attContent var depth path cls att
----------
urlstrs :: ObjectDef -> [String]
urlstrs att = [urlstr|xs<-objstrs att,'U':'R':'L':'=':urlstr<-xs]
----------------------
--display one or more pictures, assumed to be behind their url values (GEN Picture ISA Datatype)
embedimage::ObjectDef->Integer->[String]
embedimage att depth
  = if isUni(objctx att)
    then ["echo '<IMG src=\"'.$"++ phpIdentifier (name att) ++".'\"/>';"]
    else ["foreach($"++phpIdentifier (name att)++" as $i"++show depth++"=>$v"++show depth++"){"
         , "  echo '<IMG src=\"'.$v"++show depth++".'\"/>';"
         , "}"]

-----------------------------------------
--EDIT BLOCK functions
-----------------------------------------
showBlockJS::String->[Relation Concept]->ObjectDef->[String]
showBlockJS cls editable att
 = [ "function UI"++cls++"(id){"
   , "  return " ++ head attCode'] ++ (map ((++) "       + ") (tail attCode')) ++
   [ "        ;", "}"]
   where
     attCode' = map (\x->"'"++x++"'") (attCode "'+id+'." cls att)
     attCode strt c at = ["<DIV>"++(name a)++": "++(specifics c (strt ++ show n) a)++"</DIV>"
                         | (n,a)<-zip [(0::Integer)..] (objats at)]
     specifics c n a = if isUni (objctx a)
                         then if isTot (objctx a)
                              then "<SPAN CLASS=\""++itemUI editable (objctx att)++acls c a++"\" ID=\""++n
                                   ++"\">" ++ concat (attCode n (acls c a) a) ++"</SPAN>"
                              else "<DIV CLASS=\""++newUI editable (objctx att)++acls c a++"\" ID=\""
                                   ++n++"\">"++novalue++"</DIV>"
                         else "<UL><LI CLASS=\""++newUI editable (objctx att)++acls c a
                              ++"\" ID=\""++n++"\">new "++(name a)++"</LI></UL>"
     acls c a = c++"_"++(phpIdentifier (name a))
-------------
uniEditAtt::String->Integer->String->String->ObjectDef->[(String,ObjectDef)]
uniEditAtt var depth path cls att
  | null (objats att) = []
  | otherwise = concat 
       [ b | (a,n)<-zip (objats att) [(0::Integer)..]
           , b<-[attEdit (var++"['"++name a++"']") --(TODO: vgl. UniContentAtt)
                depth
                (path++"."++show n) 
                (cls ++ if length(objats att) > 1
                        then (if null cls then "" else "_") ++ phpIdentifier (name a)
                        else "")
                a]
       ]
attEdit::String->Integer->String->String->ObjectDef->[(String,ObjectDef)]
attEdit var depth path cls att 
 | not (isUni (objctx att))
   = let
     newBlocks = uniEditAtt atnm (depth+1) (path ++".'.$i"++show depth++".'") cls att
     atnm = if "$"++phpIdentifier (name att)==var then "$v"++show depth else "$"++phpIdentifier (name att)
     in
     (if null (objats att) then [] else [(cls,att)]) ++ newBlocks
 | objats att==[] = []
 | (isTot(objctx att)) = uniEditAtt var depth path cls att
 | otherwise 
   = let 
     newBlocks = uniEditAtt var depth path cls att
     tot = (isTot(objctx att))
     in 
     (if tot then [] else [(cls,att)]) ++ newBlocks

-----------------------------------------
--PHP wrapper page functions
-----------------------------------------
--phpList2Array is used once in objectWrapper:
--indentBlock 4 (concat [phpList2Array 0 ("$"++phpIdentifier (name a)) (show n) a | (a,n)<-zip (objats o) [(0::Integer)..]])
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
phpList2ArrayUni :: Int -> String->String->ObjectDef->[String]
phpList2ArrayUni depth var rqvar a
 = addToLast ";" ([ var++" = "++head (phpList2ArrayVal var rqvar a)]  ++
                  indentBlock (8 + length var) (tail (phpList2ArrayVal var rqvar a))
                 ) ++
   concat
   [ phpList2Array depth (var++"['"++name a'++"']") (rqvar++"."++show n) a'
   | (a',n)<-zip (objats a) [(0::Integer)..],not (isUni (objctx a'))]
phpList2ArrayVal :: String->String->ObjectDef->[String]
phpList2ArrayVal var rqvar a
 = if null (objats a) then ["savevalue(@$r['"++rqvar++"'])"]
   else [ "array( 'id' => savevalue(@$r['"++rqvar'++"'])"] ++ 
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
----------------------------
