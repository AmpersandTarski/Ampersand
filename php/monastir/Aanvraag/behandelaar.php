<?

/*********** file Aanvraag.adl on line 3-5:
 * OBJECT behandelaar  I[Employee]          -- 'Employee' zou betekenen: 'Employee = I[Employee]'
 * WITH aanvragen : assigned~
 * ENDOBJECT
 ***********/

class behandelaar {
	var $aanvragen;
	function behandelaar(/* behandelaar_aanvragen */ array $aanvragen){
		$this->aanvragen=$aanvragen;
	}
	function addAanvragen(behandelaar_aanvragen $aanvragen){
		$this->aanvragen[]=$aanvragen;
	}
}
class behandelaar_aanvragen {
	var $application;
	function behandelaar_aanvragen($value){
		$this->application=$value;
	}
}
function createBehandelaar(behandelaar $obj){
	echo 'create Behandelaar';
	return $id;
}
function readBehandelaar($id){
	$obj = new behandelaar(array());
	
	$obj->addAanvragen(new behandelaar_aanvragen('Aanvraag 1'));
	$obj->addAanvragen(new behandelaar_aanvragen('Aanvraag 2'));
	return $obj;
}
function updateBehandelaar($id,behandelaar $obj){
	$success=false;
	return $success;
}
function deleteBehandelaar($id){
	$success=false;
	return $success;
}

require "localsettings.inc.php";

$object = new object("behandelaar",array
                      (new oRef( new oMulti( true,false,false,false ) // derived from assigned~
                               , new object("aanvragen",array())
                               )
                      )
                    ); // meta information

$view = new view();
$view->assign("objname","Behandelaar");
$view->addAction('create','New','Create a new behandelaar');

if($action=='create'){
	$object_id=createBehandelaar($obj);
}else
if($action=='delete'){
	if(deleteBehandelaar($actionValue)){
		// succes!
		$view->addOkMessage("Behandelaar deleted");
	}else $object_id=$actionValue;
}else
if($action=='update'){
	updateBehandelaar($actionValue,$obj);
	$object_id=$actionValue;
}else
if($action=='read'){
	$object_id=$actionValue;
}

if(@$object_id){
	// show the item itself
	if($action=='read') $behandelaar = readBehandelaar($object_id);
	else $behandelaar = $obj;
	$view->assign("header",new viewableText($object_id,'H2'));
	$list=new expandableList();
	$list->assign("header",array("application"=>new viewableText("Application")));
	$list->assign("caption",new viewableText("aanvragen",'H3'));
	$list->assign("elements",$behandelaar->aanvragen);
	$list->assign("emptyRow",array("aanvragen"));
	if($action=='read' || $action=='update')
		$view->addAction('update','Save','Save the behandelaar');
	else $view->addAction('create','Create','Create this behandelaar');
	$view->assign("contents",$list);
}else{
	// show a list (or search-box) of all items
	$list=new viewableList();
	$list->assign("header",array(new viewableText("behandelaar")));
	$ctx = DB_doquer('SELECT AttE_mployee FROM C4_E_mployee');
	$elements=array();
	foreach($ctx as $i=>$v){
		$elements[]=array(new linkedText($v['AttE_mployee'],$v['AttE_mployee']));
	}
	$list->assign("elements",$elements);
	$view->assign("contents",$list);
}

$view->display();

?>