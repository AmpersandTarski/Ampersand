<?
/***************************************
 * Interface V1.3.0
 * (c) Bas Joosten June 2005-Jul 2008
 * 
 * Using htmlFunctions, SQL.lib.php
 *
 * V1.3.0 is the first version after V1.2.0
 * The filename has changed and this
 * interface is now called 'monastir'
 * instead of 'entityScreen'
 *
 * Usage of this file is now thru functions
 * This file does not 'do' anything by itself
 *
 * This file contains all code that is
 * specific for the monastir view.
 * The code is shared between apps.
 * 
 **************************************/
 
include "htmlFunctions.inc.php";

function parseRequest($object){
	global $_REQUEST;
	global $object_id;
	if(isset($_REQUEST['id'])){
		$object_id=$_REQUEST['id'];
		if(isset($_REQUEST[$object->name.'%0_'])){
			$_REQUEST[$object->name.'%0_']=$object_id;
		}
	}
	if(isset($_REQUEST[$object->name.'%0_']))
	{
		if(!isset($object_id)){
			$object_id=$_REQUEST[$object->name.'%0_'];
		}
		return recurParse($object,$object->name.'%0',$object->name);
	} else {
		return false;
	}
}
function recurParse($object,$traceID,$objtrace){
	global $_REQUEST;
	$id=@$_REQUEST[$traceID.'_'];
	$obj = new $objtrace($id);
	if($id===null) return $obj;
	foreach($object->containing as $i=>$v){
		$type=$v->type->name;
		$trace=$traceID.'%'.$type;
		for($n=0;isset($_REQUEST[$trace.'%'.$n.'_']);$n++){
			if(!isset($_REQUEST[$trace.'%'.$n]) || $_REQUEST[$trace.'%'.$n] != 'remove')
			$obj->addGen($type,recurParse($v->type,$trace.'%'.$n,$objtrace.'_'.$type));
		}
		if(isset($_REQUEST[$trace]) && $_REQUEST[$trace] == 'add'){
			$obj->addGen($type,recurParse($v->type,$trace.'%'.$n,$objtrace.'_'.$type));
		}
	}
	return $obj;
}


abstract class anyView {
	function assign($var,$val){}
	abstract function display();
}
class viewableText extends anyView{
	var $text;
	var $caption;
	var $style;
	var $id;
	function display($edit=false){
		if($edit){
			if($edit===true) $edit=$this->id;
			if(isset($this->caption))
			echo '<P>'.htmlspecialchars($this->caption).': ';
			echo '<INPUT TYPE="TEXT" CLASS="text" NAME="'.htmlspecialchars($edit).'" VALUE="'.$this->text.'" />';
			if(isset($this->caption)) echo '</P>';
		}else{
			if(isset($this->style)) echo '<'.$this->style.'>';
			echo htmlspecialchars($this->text);
			if(isset($this->style)) echo '</'.$this->style.'>';
		}
	}
	function viewableText($text,$style=null){
		$this->text=$text;
		$this->style=$style;
	}
	function assign($var,$val){
			 if($var=='id'      ) $this->id      = $val;
		else if($var=='caption' ) $this->caption = $val;
	}
}
class linkedText extends anyView{
	var $action;
	function display(){
		$this->action->display();
	}
	function linkedText($text,$link,$page=''){
		if($page>'')$page=',\''.addslashes($page).'\'';
		$link='\''.addslashes($link).'\'';
		$this->action=new action('go('.$link.$page.')',new viewableText($text),'');
	}
}
class viewableList extends anyView{
	var $elements=array();
	var $header;
	var $caption;
	var $iDir;
	var $One=false;
	var $Tot=true;
	var $wrapping=true;
	protected function displayheader($header=null){
		if(isset($this->caption)){
		 //if(is_object($this->caption)) 
		 	$this->caption->display();
		 //else echo $this->caption;
		}
		if($this->wrapping){
			echo '<TABLE class="hidden raised" height=1 width=100%><TR>';
			echo '<TD width=10   height=10 class=hidden><IMG class=hidden width=10   height=10 src="'.$this->iDir.'m/lb1.png" /></TD>';
			echo '<TD width=100% height=10 class=hidden><IMG class=hidden width=100% height=10 src="'.$this->iDir.'m/zijde_b1.png" /></TD>';
			echo '<TD width=10   height=10 class=hidden><IMG class=hidden width=10   height=10 src="'.$this->iDir.'m/rb1.png" /></TD>';
			echo '</TR><TR>';
			echo '<TD width=10 height=100% class=hidden><IMG class=hidden width=10 height=100% src="'.$this->iDir.'m/zijde_l1.png" /></TD>';
			echo '<TD width=100% height=100% class=hidden>';
		}
		$colspan=0;
		if(!$this->One){
			echo "\r\n".'<TABLE height=1 width=100% class="hidden">';
			if(isset($header) && count($header)){
				echo "\r\n".'<TR>';
				$colspan=0;
				foreach($header as $j=>$h){
					$colspan++;
					echo '<TH>';
					//if(is_object($h))
					$h->display();
					//else echo '[no display object]';
					echo '</TH>';
					$colspan++;
				}
				echo '</TR>';
				if($this->wrapping){
					echo '<TR><TD colspan='.$colspan.' class=hidden><IMG class=hidden width=100% height=10 src="'.$this->iDir.'m/zijde_b1.png" /></TD></TR>';
				}
			}
		}
		return $colspan;
	}
	function display(){
		$colspan=$this->displayheader($this->header);
		if(!isset($this->header) && count($this->elements)) $header=$this->elements[0];
		if(count($this->elements)){
			foreach($this->elements as $i=>$v){
				if(!$this->One){
					echo "\r\n  ".'<TR>';
					foreach($this->header as $j=>$h){
						echo "\r\n    ".'<TD>';
						$v[$j]->display();
						echo '</TD>';
					}
					echo '</TR>';
				}else{
					foreach($this->header as $j=>$h){
						$h->style='h4';
						$h->display();
						echo '<P>';
						$v[$j]->display();
						echo '</P>';
					}
				}
			}
		}else{
			$this->dispNone($colspan);
		}
		$this->displaytail();
	}
	function dispNone($colspan=0){
		if(!$this->One){
			echo '<TR><TD';
			if($colspan>0){
				echo ' colspan='.$colspan;
			}
			echo '>';
		} else echo '<P>';
		echo '<I>None</I>';
		if(!$this->One) echo '</TD></TR>'; else echo '</P>';
	}
	function displaytail(){
		if(!$this->One){
			echo "</TABLE>";
		}
		if($this->wrapping){
			echo '</TD>';
			echo '<TD width=10 height=100% class=hidden><IMG class=hidden width=10 height=100% src="'.$this->iDir.'m/zijde_r1.png" /></TD>';
			echo '</TR><TR>';
			echo '<TD width=10   height=10 class=hidden><IMG class="hidden" width=10   height=10 src="'.$this->iDir.'m/lo1.png" /></TD>';
			echo '<TD width=100% height=10 class=hidden><IMG class="hidden" width=100% height=10 src="'.$this->iDir.'m/zijde_o1.png" /></TD>';
			echo '<TD width=10   height=10 class=hidden><IMG class="hidden" width=10   height=10 src="'.$this->iDir.'m/ro1.png" /></TD>';
			echo '</TR><TR>';
			echo '</TABLE>';
		}
	}
	function assign($var,$val){
		if($var=='header'){
			$this->header=$val;
		}else if($var=='elements'){
			$this->elements=$val;
		}else if($var=='caption'){
			$this->caption=$val;
		}else if($var=='iDir'){
			$this->iDir=$val;
		}else if($var=='One'){
			$this->One=$val;
		}else if($var=='Tot'){
			$this->Tot=$val;
		}
	}
	function viewableList(){
		global $imageDirName;
		$this->iDir=$imageDirName;
	}
}
class expandableList extends viewableList{
	static $itemID=0;
	var $object;
	var $traceID;
	//private $header;
	function assign($var,$val){
		if($var=='object'){
			$this->object=$val;
		}else parent::assign($var,$val);
	}
	function display($edit=false,$traceID=null){
		if(isset($traceID))
			$this->traceID=$traceID;
		else
			$this->traceID=$this->object->name;
		$this->header= array();
		foreach($this->object->containing as $i=>$v){
			$this->header[$i]=new viewableText($v->type->name);
		}
		if(count($this->object->containing) <= 1) $this->wrapping=false;
		if(count($this->object->containing) == 0) $this->header[]=new viewableText('');
		$this->header[]=new viewableText('');
		$this->displayheader($this->header);
		if(count($this->elements)){
			foreach($this->elements as $i=>$v){
				$this->displayRow($v,$edit,$i,count($this->elements));
			}
		}else if(!$this->Tot){
			$this->dispNone(count($this->header));
		}else if($edit){ // edit should allways be true, or this wouldn't have happened
			$this->displayRow(null,$edit,0,count($this->elements));
		}
		if($edit && (!$this->One)) $this->displayEmptyRow($this->header);
		$this->displaytail();
	}
	function displayRow($row=null,$edit=false,$rowId=0,$totRow=0){
		if(!$this->One)
			echo "\r\n  <TR>";
		if(isset($row->id))$id=$row->id; else $id='';
		$page=@$this->object->page;
		if($edit){
			$rowID=$this->traceID.'%'.$rowId;
		}
		if(count($this->object->containing)){ // if the object has sub-objects
			foreach($this->object->containing as $j=>$v){
				$type=$v->type->name;
				if(!$this->One)
					echo "<TD>";
				else {
					$h=new viewableText($v->type->name);
					$h->style='h4';
					$h->display();
					if($v->mult->uni) echo '<P>';
				}
				$myRow = new expandableList();
				$myRow->assign("One",$v->mult->uni);
				$myRow->assign("Tot",$v->mult->tot);
				$myRow->assign('object',$v->type);
				if(isset($row))
				$myRow->assign('elements',$row->$type);
				else $myRow->assign('elements',array());
				if($edit) $myRow->display(true,$rowID.'%'.$type);
				else $myRow->display();
				if(!$this->One)
					echo "</TD>";
				else if($v->mult->uni) echo '</P>';
			}
			if($edit){
				echo '<INPUT TYPE="hidden" NAME="'.htmlspecialchars($rowID).'_" VALUE="'.htmlspecialchars($id).'" />';
			}
		}else{
			if(!$this->One)
				echo '<TD>';
			$myTxt = new viewableText($id);
			if($edit){
				$myTxt->assign("id",$rowID.'_');
				//$myTxt->assign('default',$id);
				$myTxt->display($edit);
			}else{
				if(isset($page)) echo '<A HREF="JavaScript:go(\''.htmlspecialchars(addslashes($id).'\',\''.addslashes($page)).'\');">';
				$myTxt->display();
				if(isset($page)) echo '</A>';
			}
			if(!$this->One)
				echo '</TD>';
		}
		if($edit && !$this->One){
			if(!$this->Tot || $totRow>1){
				echo '<TD align=left><input type="image" name="'.htmlspecialchars($rowID).'" value="remove" src="'.$this->iDir.'remove.png" /></TD>';
			}else echo '<TD></TD>';
		}else if($edit && (!$this->Tot || $totRow>1)){
			echo '<input type="image" name="'.htmlspecialchars($rowID).'" value="remove" src="'.$this->iDir.'remove.png" style="display:block" />';
		}
		if(!$this->One)
			echo "</TR>";
	}
	function displayEmptyRow($row){
		$colspan=max(1,count($this->object))+1;
		if(!$this->One) echo '<TR><TD colspan='.$colspan.'>';
		//echo '<button name="'.$this->traceID.'" value="add">Add</button>';
		echo '<input type="image" name="'.htmlspecialchars($this->traceID).'" value="add" SRC="'.$this->iDir.'add.png" />';
		if(!$this->One) echo '</TD></TR>';
	}
}
class removableListRow extends anyView{
	function display(){
		
	}
}
class monastir Extends anyView {
	var $iDir='infeez/';
	var $menu=Array();
	var $contents;
	var $action;
	var $actions=Array();
	var $warning=Array();
	var $header;
	var $appname='Monastir Databaseviewer';
	var $objname;
	var $object_id;
	var $defaultAction;
	var $succes=true;
	var $ok;
	function monastir($menu,$obj,$object,$cObjName){
			global $action;
			global $imageDirName;
			global $appName;
			$this->appname=$appName;
			$this->objname=$cObjName;
			$this->iDir=$imageDirName;
			$this->menu=$menu;
			global $_REQUEST;
			$changed=true;
			if(isset($_REQUEST['read'])){
				$changed=false;
				$action='read';
				$object_id=$_REQUEST['read'];
				$f='read'.$cObjName;
				$obj= $f($object_id); // from DB
				if($object_id===false){
					$this->assign("succes",false);
					$action='show';
				}
			} else if(isset($_REQUEST['new']) || @$_POST['action']=='new'){
				$action='new';
				$defaultAction='create';
				if($obj===false) { // not false after edit!
					$f=$object->name;
					$obj=new $f(); // return an empty object
				}
				$object_id=@$_POST['id'];
			} else if(@$_POST['action']=='create'){
				$action='create';
				$obj->id=$_POST['id'];
				if(isset($obj->id) && $obj->id=='') {$obj->id=null;}
				$f='create'.$cObjName;
				$object_id= $f($obj);
				if($object_id===false) {$object_id=$obj->id;$this->assign("succes",false);} else $this->assign("succes",true);
			} else if(@$_POST['action']=='update'){
				$action='update';
				$f='update'.$cObjName;
				$object_id= $f($obj);
				if($object_id===false) {$this->assign("succes",false);
				} else $this->assign("succes",true);
			} else if(isset($_REQUEST['edit']) || @$_POST['action']=='edit'){ // no changes (yet)! Fake a read.
				$action='edit';
				$defaultAction='update';
				if($obj==false){
					$f='read'.$cObjName;
					$obj= $f($_REQUEST['edit']); // from DB
				}
				$object_id=$obj->id;
			} else if(isset($_REQUEST['delete'])){
				$action='delete';
				$f='delete'.$cObjName;
				if($f($_REQUEST['delete'])){
					$this->assign("succes",true);
					$this->assign("ok",new viewableText($cObjName." deleted",'H4'));
				}else{
					$this->assign("succes",false);
					$object_id=$_REQUEST['delete'];
					$f='read'.$cObjName;
					$obj= $f($object_id); // from DB
				}
			} else { $action='show'; $changed=false; }
			if(isset($defaultAction)) $this->defaultAction=$defaultAction;
			else $this->defaultAction=$action;
			$this->action=$action;
			$this->changed=$changed;
			if($obj){ // read on no valid object id: send empty object
				// show the item itself
				$header = new viewableText(@$object_id,'H2');
				$header->assign("caption",$cObjName);
				$this->assign("header",$header);
				$list=new expandableList();
				$list->assign("object",$object);
				$list->assign("elements",array($obj));
				$list->assign("One",true);
				if(isset($object_id)) $this->assign("object_id",$object_id);
				$this->assign("contents",$list);
			}else{
				// show a list (or search-box) of all items
				$list=new viewableList();
				$list->assign("header",array(new viewableText($cObjName)));
				$f='getEach'.$cObjName;
				$ctx = $f();
				$elements=array();
				foreach($ctx as $i=>$v){
					$elements[]=array(new linkedText($v[0],$v[0]));
				}
				$list->assign("elements",$elements);
				$this->assign("contents",$list);
				$list->wrapping=false;
			}
			//$list->wrapping=false;
	}
	function assign($var,$val){
		if     ($var=='actions'   ) $this->actions   = $val;
		else if($var=='appname'   ) $this->appname   = $val;
		else if($var=='objname'   ) $this->objname   = $val;
		else if($var=='object_id' ) $this->object_id = $val;
		else if($var=='contents'  ) $this->contents  = $val;
		else if($var=='header'    ) $this->header    = $val;
		else if($var=='succes'    ) $this->succes    = $val;
		else if($var=='ok'        ) $this->ok        = $val;
		else $this->warning[] = $var.' was not assigned.';
	}
	function display(){
		global $_REQUEST;
		global $DB_err;
		//global $changed;
		//$this->changed=$changed;
		//if($changed) echo 'c';
		$action=$this->action;
		$edit=false;
		if(isset($_REQUEST['action'])) $changed=true;
		//if($changed) echo 'c'; else echo 'n';
		if($action=='read'){
			if(isset($_REQUEST['edit']) || @$_REQUEST['action']=='edit') $action = 'edit'; // was faked as read
			if(!isset($this->object_id)) $action = 'new'; // was faked as read
		}
		if($action=='create') $action = $this->succes ? 'read' : 'new';
		if($action=='delete') $action = $this->succes ? 'show' : 'read';
		if($action=='update') $action = $this->succes ? 'read' : 'edit';
		$qobj='\''.addslashes($this->object_id).'\'';
		if($action=='read'){
			$this->addAction('g(\'edit\','.$qobj.')','Edit','Edit '.$this->objname.' '.$this->object_id);
			$this->addAction('g(\'delete\','.$qobj.')','Delete','Delete '.$this->objname.' '.$this->object_id);
			$this->addAction('g(\'new\')','New','Create a new '.$this->objname);
			$this->addAction('g(\'show\')','Overview','Show all '.$this->objname.' objects');
		}else if($action=='new'){
			$defaultAction='create';
			$edit=true;
			$this->addAction('s(\'create\')','Create','Create the '.$this->objname);
			$this->addAction('g(\'show\')','Cancel','Don\'t create the '.$this->objname);
		}else if($action=='edit'){
			$defaultAction='update';
			$edit=true;
			$this->addAction('s(\'update\')','Save','Save the '.$this->objname);
			$this->addAction('g(\'read\','.$qobj.')','Cancel','Don\'t save the '.$this->objname);
		}else if($action=='show'){
			$this->addAction('g(\'new\')','New','Create a new '.$this->objname);
		}
		$title=$this->appname;
		if(isset($this->objname))
		$title=$this->objname." - ".$title;
		$menu=$this->menu;
		$actions=$this->actions;
		$contents=$this->contents;
		$iDir=$this->iDir;
		$changed=$this->succes && $edit;
		?><!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd">
		<HTML><HEAD>
		<?	writeTitle($title);
			writeCSS_blue(); ?>
		<SCRIPT LANGUAGE="JavaScript">
		<!--
			var changed=<?= $changed ? 'true' : 'false' ?>;
			<? if(isset($this->object_id)){ ?>
			var obj='<?=addslashes($this->object_id) ?>';
			<? } ?>
			function go(id,page,what){ // this is actually 'read' when id is given
				if(what==null) what='read';
				if(changed){
					if(!confirm("Your changes are not saved. Do you still want to leave this page?")){
						//document.forms['myform'].submit();
						return void(0);
					}else changed=false;
				}
				if(changed==false){
					if(page==null) g(what,id);
					else{
						if(id==null)id='';
						else id='?'+what+'='+escape(id);
						document.location = page+id;
					}
				}
			}
			function g(what,id){
				if(id==null)id='?'+what;
				else id='?'+what+'='+escape(id);
				document.location = id;
			}
			function s(what){
				document.forms.myform.action.value=what;
				document.forms.myform.submit();
			}
			function toggle(i){
				item=document.getElementById(i);
				if(item.style.visibility=='hidden'){
					item.style.visibility='visible';
					item.style.position='relative';
				}else{
					item.style.visibility='hidden';
					item.style.position='absolute';
				}
			}
		//-->
		</SCRIPT>
		</HEAD><BODY STYLE="height:100%;width:100%;" background="<?=$iDir ?>blue.png" marginwidth="0" marginheight="0"<?
		if(isset($focusField)&&$focusField!='') echo ' onload="document.forms.'.$focusField.'.focus();"';
		?> topmargin="0" leftmargin="0" bottommargin="0" rightmargin="0"><TABLE CLASS="hidden" HEIGHT=100% WIDTH=100%><TR><TD VALIGN=MIDDLE ALIGN=CENTER height=100% style="height:100%">
		
		<?
		if(count($this->warning)) echo join('<BR>',$this->warning);
		$this->warning=Array();
		echo '<TABLE class="hidden" height=1><TR class="hidden" height=100%>';
		if(count($menu)){
			echo "\r\n<!-- menu -->\r\n  ";
			echo '<TD class="hidden" width=20% height=100 valign=top>';
				echo '<TABLE WIDTH=100% HEIGHT=100% class="hidden">';
				echo '<TR>';
				echo '<TD class="hidden" height=60 width=27><IMG src="'.$iDir.'l/hoek_lboven.png" height=60 width=27 border=0 /></TD>';
				echo '<TD class="hidden" height=60 width=100% background="'.$iDir.'l/zijde_boven.png"><IMG src="'.$iDir.'l/zijde_boven.png" height=60 width=38 border=0 /></TD>';
				echo '<TD class="hidden" height=60 width=24><IMG src="'.$iDir.'l/hoek_rboven.png" height=60 width=24 border=0 /></TD>';
				echo '</TR>';
				$first=true;
				foreach($menu as $i=>$component){
					if(!$first){
						echo "\r\n  ".'<TR>';
						echo '<TD class="hidden" height=8 width=27 border=0 /><IMG src="'.$iDir.'l/bar_links.png" height=8 width=27 border=0 /></TD>';
						echo '<TD class="hidden" height=8 background="'.$iDir.'l/bar_midden.png" ><IMG src="'.$iDir.'l/bar_midden.png" height=8 width=38 border=0 /></TD>';
						echo '<TD class="hidden" height=8 width=24 border=0 /><IMG src="'.$iDir.'l/bar_rechts.png" height=8 width=24 border=0 /></TD></TR>';
					}
					echo "\r\n  ".'<TR>';
					if(is_array($component)){
						foreach($component as $i=>$v){
							if($v->selected){
								echo "\r\n  ".'<TR>';
								echo '<TD class="hidden" width=27 background="'.$iDir.'l/zijde_links_sel.png" border=0 /><IMG src="'.$iDir.'l/zijde_links_sel.png" height=21 width=27 border=0 /></TD>';
								echo '<TD class="hidden" background="'.$iDir.'l/bg_sel.png" valign=middle>';
								$v->display();
								echo '</TD><TD class="hidden" width=24 background="'.$iDir.'l/zijde_rechts_sel.png" border=0 /><IMG src="'.$iDir.'l/zijde_rechts_sel.png" height=24 width=24 border=0 /></TD>';
								echo '</TR>';
							}else{
								echo "\r\n  ".'<TR>';
								echo '<TD class="hidden" width=27 background="'.$iDir.'l/zijde_links.png" border=0 /><IMG src="'.$iDir.'l/zijde_links.png" height=21 width=27 border=0 /></TD>';
								echo '<TD class="hidden" bgcolor=white valign=middle>';		
								$v->display();
								echo '</TD><TD class="hidden" width=24 background="'.$iDir.'l/zijde_rechts.png" border=0 /><IMG src="'.$iDir.'l/zijde_rechts.png" height=24 width=24 border=0 /></TD>';
								echo '</TR>';
							}
						}
					}else{
						echo '<TD class="hidden" width=27 background="'.$iDir.'l/zijde_links.png" border=0 /><IMG src="'.$iDir.'l/zijde_links.png" height=21 width=27 border=0 /></TD>';
						echo '<TD class="hidden" bgcolor=white valign=middle>';
						$component->display();
						echo '</TD><TD class="hidden" width=24 background="'.$iDir.'l/zijde_rechts.png" border=0 /><IMG src="'.$iDir.'l/zijde_rechts.png" height=24 width=24 border=0 /></TD>';
					}
					echo '</TR>';
					$first=false;
				}
				echo '</TABLE>';
			echo '</TD>'."\r\n";
		}else{
			echo '<TD class="hidden" width=20% height=29 align="right"><IMG SRC="'.$iDir.'l/zijde_links2.png" height=29 width=24 /></TD>';
			// use another pic for this..
		}
		echo '<TD class="hidden" width=80% rowspan=3 valign=top style="height:100%" height=100%>';
			//echo '<SPAN class="hidden" style="display:clip">';
			echo '<TABLE CLASS="hidden" height="100%" width="100%">';
			echo '<TR>';
			echo '<TD class="hidden" height=36 width=33><IMG SRC="'.$iDir.'h/hoek_lboven.png" width=33 height=36/></TD>';
			echo '<TD class="hidden" height=36 width=100% background="'.$iDir.'h/zijde_boven.png"><IMG SRC="'.$iDir.'spacer.gif" width=1 height=36/></TD>';
			echo '<TD class="hidden" height=36 width=100% width=60><IMG SRC="'.$iDir.'h/hoek_rboven.png" width=60 height=36/></TD>';
			echo '</TR>';
			echo "\r\n".'<TR height="100%" style="height:100%">';
			echo '<TD class="hidden" height="100%" style="height:100%" width=33 background="'.$iDir.'h/zijde_links.png"><img src="'.$iDir.'spacer.gif" width=33 height=1 /></TD>';
			echo '<TD class="hidden" height="100%" width=100% bgcolor=white valign=top>';
			if($this->succes){
				if(isset($this->ok)) $this->ok->display();
			}else{
				$err = new viewableText($DB_err,'EM');
				$err->display();
			}
			echo "\r\n<!-- contents -->";
			echo "\r\n  ";
			if($edit){
				echo '<FORM NAME="myform" ID="myform" METHOD="POST" ';
				echo 'onkeydown="if((window.event && event.keyCode==13)||';
					echo '(!window.event && event.which==13)){';
						echo 'this.action.value=\''.$this->defaultAction.'\';this.submit();return true;';
						echo '}else return void(0);"';
				echo ' AUTOCOMPLETE="off" ACTION="'.$_SERVER['SCRIPT_NAME'].'">';
				//echo '<DIV style="visibility:hidden;position:absolute"><INPUT TYPE="submit" name="action" value="'.$this->defaultAction.'" /></DIV>';
				echo '<INPUT TYPE="HIDDEN" NAME="action" VALUE="'.$action.'">';
				//echo '<INPUT TYPE="HIDDEN" NAME="defaultAction" VALUE="'.$defaultAction.'">';
				//if($action=='new'){
				//	echo '<INPUT TYPE="HIDDEN" NAME="id" VALUE="'.$this->object_id.'">';
				//}
			}
			if(isset($this->header))
			{
				$this->header->assign("id","id");
				if($action=='new') $this->header->display($edit);
				else $this->header->display(false);
				if(!isset($this->header->style)) echo '<BR />';
			}
			$contents->display($edit);
			if($edit){
				echo '</FORM>';
			}
			echo "\r\n</TD>";
			echo '<TD class="hidden" height="100%" width=60 background="'.$iDir.'h/zijde_rechts.png"><img src="'.$iDir.'spacer.gif" width=60 height=1 /></TD>';
			echo '</TR>';
			echo "\r\n<TR>";
			echo '<TD class="hidden" width=33 height=73><IMG SRC="'.$iDir.'h/hoek_londer.png" border=0 width=33 height=73 /></TD>';
			echo '<TD class="hidden" width=100% height=73 background="'.$iDir.'h/zijde_onder.png" align=center>';
			$shown=false;
			if(count($actions)){
				echo "\r\n<!-- Buttons -->\r\n  <TABLE class=\"hidden\"><TR>";
				foreach($actions as $i=>$component){
					echo "\r\n  ".'<TD class="hidden" >';
					echo '<TABLE height=73 class="hidden">'
							.'<TR><TD align=right class="hidden" background="'.$this->iDir.'h/zijde_onder1.png" height=29>'
							.'<IMG SRC="'.$this->iDir.'h/zijde_onder1.png" border=0 height=29 />'
							.'</TD></TR>'
							.'<TR><TD class="hidden">'
							//.'<A HREF="JavaScript:'.$component->jsaction.';" title="'.htmlspecialchars($component->description).'">'
							.'<TABLE height=27 class="hidden" style="cursor:pointer" title="'.$component->description.'" onClick="'.$component->jsaction.';">'
								.'<TR><TD class="hidden" height=27 width=12 align="right">'
								  .'<IMG class="hidden" SRC="'.$this->iDir.'knop_links.png" height=27 align=left width=12 />'
								.'</TD>'
								.'<TD height=27 align="center" class="hidden" background="'.$this->iDir.'/knop_midden.png" class="hidden" style="overflow:hidden;">'
								  .'<small class="button"><b>'.htmlspecialchars($component->caption).'</b></small>'
								.'</TD>'
								.'<TD height=27 class="hidden" align="left" width=12>'
								  .'<IMG class="hidden" SRC="'.$this->iDir.'knop_rechts.png" height=27 align=right />'
								.'</TD></TR>'
							.'</TABLE>'
							//.'</A>'
							.'</TD></TR>'
							.'<TR><TD align=left class="hidden" background="'.$this->iDir.'h/zijde_onder3.png" height=17>'
							.'<IMG SRC="'.$this->iDir.'h/zijde_onder3.png" border=0 height=17 />'
							.'</TD></TR>'
						.'</TABLE>';
					$shown=true;
					echo '</TD>';
				}
				echo "\r\n</TR></TABLE>\r\n";
			}
			if(!$shown){
				echo '<IMG SRC="'.$iDir.'h/zijde_onder.png" border=0 width=45 height=73 />';
			}
			echo '</TD>';
			echo '<TD class="hidden" width=60 height=73><IMG SRC="'.$iDir.'h/hoek_ronder.png" border=0 width=60 height=73 />';
			echo '</TD>';
			echo '</TABLE>';
			//echo '</SPAN>';
		echo '</TD>';
		echo '</TR>';
		echo '<TR><TD class=hidden width=20% height=100%>';
			echo '<TABLE class="hidden" height=100% width=100%>';
			echo '<TD class="hidden" width=27 height=100% background="'.$iDir.'blue.png" valign=top><IMG SRC="'.$iDir.'l/hoek_londer.png" border=0 width=27 height=40 /></TD>';
			echo '<TD class="hidden" background="'.$iDir.'blue.png" valign=top height=100%>';
			echo '<TABLE height=100% width=100% class="hidden">';
			echo '<TR><TD height=40 background="'.$iDir.'l/zijde_onder.png" class="hidden">';
			echo '<IMG SRC="'.$iDir.'l/zijde_onder.png" height=40 /></TD>';
			echo '</TR><TR><TD height=100% width=100% class="hidden" background="'.$iDir.'blue.png"><img src="'.$iDir.'spacer.gif" height=1 width=1 border=0 />';
			echo '</TD></TR></TABLE>';
			echo '</TD>';
			echo '<TD class="hidden" height=100% width=24 valign=top background="'.$iDir.'h/zijde_links2.png"><IMG SRC="'.$iDir.'l/koppel_onder.png" border=0 width=24 height=40 /></TD>';
			echo '</TR>';
			echo '</TABLE>';
		echo '</TD></TR>';
		echo '<TR><TD class="hidden" background="'.$iDir.'blue.png" align="right" valign=bottom>';
			echo '<TABLE WIDTH=100% height=100% class="hidden"><TR><TD></TD><TD class="hidden" background="'.$iDir.'h/zijde_links2.png" height=100%></TD></TR>';
			echo '<TR><TD></TD><TD height=73 width=24 class="hidden"><IMG SRC="'.$iDir.'h/zijde_linksonder2.png" width=24 height=73 /></TD></TR></TABLE>';
		echo '</TABLE>';
		?>
		<CENTER>
		<small><a class="cNotice" title="&copy; Sebastiaan JC Joosten 2005-2008">Layout V1.3.0 alpha</A></small>
		</TD></TR></TABLE>
		</BODY></HTML>
		<?
		if(count($this->warning)) echo join("\r<br />\r",$this->warning);
		$this->warning=array();
	}
	function icon($url,$title=""){
		$iDir=$this->iDir;
		echo "<IMG SRC=\"".$iDir."icons/".$url.".png\" TITLE=\"$title\" BORDER=\"0\" \>";
	}
	function JSLink($code,$title,$class,$link){
		$title=htmlspecialchars($title);
		$link=$link; // can be an image
		$class=htmlspecialchars($class);
		echo "<A HREF=\"JavaScript:$code\" TITLE=\"$title\" class=\"$class\" />$link</A>";
	}
	function addAction($action,$caption,$description){
		$this->actions[]=new action($action,$caption,$description);
	}
}

class action extends anyView{
	var $jsaction;var $caption;var $description;
	function action($jsaction,$caption,$description){
		$this->jsaction=$jsaction;
		$this->caption=$caption;
		$this->description=$description;
	}
	function display(){
		echo '<A HREF="JavaScript:'.$this->jsaction.';" title="'.htmlspecialchars($this->description).'">';
		if(is_string($this->caption)) echo $this->caption; else $this->caption->display();
		echo '</A>';
	}
	function assign($var,$val){
		if($var=='action')$this->jsaction=$val;
		if($var=='caption')$this->caption=$val;
		if($var=='description')$this->description=$val;
	}
}

class menuItem {
	var $link;
	var $url;
	var $title;
	var $selected=false;
	var $class;
	var $warning=array();
	function assign($var,$val){
		if     ($var=='link' ) $this->link  = $val;
		else if($var=='url'  ) {
			$this->url   = $val;
			$pp = pathinfo($_SERVER['PHP_SELF']);
			if($val == $_SERVER['PHP_SELF'] || $val==$pp['basename']) $this->selected=true;
		}
		else if($var=='title') $this->title = $val;
		else if($var=='class') $this->class = $val;
		else $this->warning[] = $var.' cannot be assigned.';
	}
	function menuItem($url="#",$title="",$class="",$link="???"){
		$this->link=$link;$this->title=$title;$this->class=$class;
		$this->assign('url',$url);
	}
	function display(){
		$url='null,\''.htmlspecialchars(addslashes($this->url)).'\'';$title=$this->title;$class=$this->class;$link=$this->link;
		
		echo "\r	";
		$this->showLink($url,$title,$class,$link);
	}
	function showLink($url,$title,$class,$link){
		$title=htmlspecialchars($title);
		$link=$link;
		$class=htmlspecialchars($class);
		if($this->selected) $class.=' selected';
		echo "<A HREF=\"JavaScript:go($url)\" TITLE=\"$title\" class=\"$class\" />$link</A>";
	}
}

?>