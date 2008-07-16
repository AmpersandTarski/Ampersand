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

if(isset($_REQUEST['read'])){
	$action='read';
	$actionValue=$_REQUEST['read'];
} else if(isset($_REQUEST['create'])){
	$action='create';
} else if(isset($_REQUEST['update'])){
	$action='update';
	$actionValue=$_REQUEST['update'];
} else if(isset($_REQUEST['delete'])){
	$action='delete';
	$actionValue=$_REQUEST['delete'];
} else $action='show';

abstract class anyView {
	function assign($var,$val){}
	abstract function display();
}
class viewableText extends anyView{
	var $text;
	var $style;
	function display(){
		if(isset($this->style)) echo '<'.$this->style.'>';
		echo htmlspecialchars($this->text);
		if(isset($this->style)) echo '</'.$this->style.'>';
	}
	function viewableText($text,$style=null){
		$this->text=$text;
		$this->style=$style;
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
	protected function displayheader($header=null){
		if(isset($this->caption)){
		 //if(is_object($this->caption)) 
		 	$this->caption->display();
		 //else echo $this->caption;
		}
		echo "\r\n".'<TABLE>';
		$colspan=0;
		if(isset($header)){
			echo "\r\n".'<TR>';
			foreach($header as $j=>$h){
				echo '<TH>';
				//if(is_object($h))
				$h->display();
				//else echo '[no display object]';
				echo '</TH>';
				$colspan++;
			}
			echo '</TR>';
		}
		return $colspan;
	}
	function display(){
		$this->displayheader($this->header);
		if(!isset($this->header) && count($this->elements)) $header=$this->elements[0];
		if(count($this->elements)){
			foreach($this->elements as $i=>$v){
				echo "\r\n  ".'<TR>';
				foreach($this->header as $j=>$h){
					echo "\r\n    ".'<TD>';
					$v[$j]->display();
					echo '</TD>';
				}
				echo '</TR>';
			}
		}else{
			echo '<TR><TD';
			if($colspan>0){
				echo ' colspan='.$colspan;
			}
			echo '><I>None</I></TD></TR>';
		}
		echo "</TABLE>";
	}
	function assign($var,$val){
		if($var=='header'){
			$this->header=$val;
		}else if($var=='elements'){
			$this->elements=$val;
		}else if($var=='caption'){
			$this->caption=$val;
		}
	}
}
class expandableList extends viewableList{
	var $emptyRow;
	static $itemID=0;
	function assign($var,$val){
		if($var=='emptyRow'){
			$this->emptyRow=$val;
		} else parent::assign($var,$val);
	}
	function display(){
		$header=$this->header;
		$header[]=new viewableText('');
		$this->displayheader($header);
		foreach($this->elements as $i=>$v){
			$this->displayRow($v);
		}
		$this->displayEmptyRow($this->emptyRow);
		echo "</TABLE>";
	}
	function displayRow($row){
		echo "\r\n  <TR>";
		foreach($this->header as $i=>$v){
			echo "<TD>";
			echo $row->$i;
			echo "</TD>";
		}
		echo "</TR>";
	}
	function displayEmptyRow($row){
		echo '<TR><TD>emptyRow</TD></TR>';
	}
	function expandableList(){
	//	echo 'expandable';
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
	var $actions=Array();
	var $warning=Array();
	var $header;
	var $appname='Monastir Databaseviewer';
	var $objname;
	function monastir(){
	}
	function assign($var,$val){
		if     ($var=='menu'    ) $this->menu     =$val;
		else if($var=='actions' ) $this->actions  =$val;
		else if($var=='iDir'    ) $this->iDir     =$val;
		else if($var=='appname' ) $this->appname  =$val;
		else if($var=='objname' ) $this->objname  =$val;
		else if($var=='contents') $this->contents =$val;
		else if($var=='header'  ) $this->header   =$val;
		else $this->warning[] = $var.' was not assigned.';
	}
	function display(){
		$title=$this->appname;
		if(isset($this->objname))
		$title=$this->objname." - ".$title;
		$menu=$this->menu;
		$actions=$this->actions;
		$contents=$this->contents;
		$iDir=$this->iDir;
		?><!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd">
		<HTML><HEAD>
		<?	writeTitle($title);
			writeCSS_blue(); ?>
		<SCRIPT LANGUAGE="JavaScript">
		<!--
			var changed=false;
			function go(id,page){
				if(changed){
					if(confirm("Do you wish to save your changes")){
						//document.forms['myform'].submit();
						alert('sorry not saved');
					}else changed=false;
				}
				if(changed==false){
					if(id==null)id='';
					else id='?read='+escape(id);
					if(page==null) page='';
					document.location = page+id;
				}
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
		echo '<TABLE class="hidden" ><TR class="hidden">';
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
					echo '<TD class="hidden" width=27 background="'.$iDir.'l/zijde_links.png" border=0 /><IMG src="'.$iDir.'l/zijde_links.png" height=21 width=27 border=0 /></TD>';
					echo '<TD class="hidden" bgcolor=white valign=top>';
					if(!$first) echo '<BR />';
					if(is_array($component)){
						$second=false;
						foreach($component as $i=>$v){
							if($second){
								echo '<TD class="hidden" width=24 background="'.$iDir.'l/zijde_rechts.png" border=0 /><IMG src="'.$iDir.'l/zijde_rechts.png" height=24 width=24 border=0 /></TD>';
								echo '</TR>';
								echo "\r\n  ".'<TR>';
								echo '<TD class="hidden" width=27 background="'.$iDir.'l/zijde_links.png" border=0 /><IMG src="'.$iDir.'l/zijde_links.png" height=21 width=27 border=0 /></TD>';
								echo '<TD class="hidden" bgcolor=white valign=top>';							
							} else $second=true;
							$v->display();
						}
					}else $component->display();
					echo '<TD class="hidden" width=24 background="'.$iDir.'l/zijde_rechts.png" border=0 /><IMG src="'.$iDir.'l/zijde_rechts.png" height=24 width=24 border=0 /></TD>';
					echo '</TR>';
					$first=false;
				}
				echo '</TABLE>';
			echo '</TD>'."\r\n";
		}else{
			echo '<TD class="hidden" width=20% height=29 align="right"><IMG SRC="'.$iDir.'l/zijde_links2.png" height=29 width=24 /></TD>';
			// use another pic for this..
		}
		echo '<TD class="hidden" width=80% height=100% rowspan=3 valign=top>';
			echo '<TABLE CLASS="hidden" height=100%>';
			echo '<TR>';
			echo '<TD class="hidden" height=36 width=33><IMG SRC="'.$iDir.'h/hoek_lboven.png" width=33 height=36/></TD>';
			echo '<TD class="hidden" height=36 width=100% background="'.$iDir.'h/zijde_boven.png"><IMG SRC="'.$iDir.'spacer.gif" width=1 height=36/></TD>';
			echo '<TD class="hidden" height=36 width=100% width=60><IMG SRC="'.$iDir.'h/hoek_rboven.png" width=60 height=36/></TD>';
			echo '</TR>';
			echo "\r\n<TR>";
			echo '<TD class="hidden" width=33 background="'.$iDir.'h/zijde_links.png"><img src="'.$iDir.'spacer.gif" width=33 height=1 /></TD>';
			echo '<TD class="hidden" width=100% bgcolor=white valign=top>';
			echo "\r\n<!-- contents -->";
			echo "\r\n  ";
			if(isset($this->header))
			{
				$this->header->display();
				if(!isset($this->header->style)) echo '<BR />';
			}
			$contents->display();
			echo "\r\n</TD>";
			echo '<TD class="hidden" width=60 background="'.$iDir.'h/zijde_rechts.png"><img src="'.$iDir.'spacer.gif" width=60 height=1 /></TD>';
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
	var $class;
	var $warning=array();
	function assign($var,$val){
		if     ($var=='link' ) $this->link  = $val;
		else if($var=='url'  ) $this->url   = $val;
		else if($var=='title') $this->title = $val;
		else if($var=='class') $this->class = $val;
		else $this->warning[] = $var.' cannot be assigned.';
	}
	function menuItem($url="#",$title="",$class="",$link="???"){
		$this->link=$link;$this->url=$url;$this->title=$title;$this->class=$class;
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
		echo "<A HREF=\"JavaScript:go($url)\" TITLE=\"$title\" class=\"$class\" />$link</A>";
	}
}

?>