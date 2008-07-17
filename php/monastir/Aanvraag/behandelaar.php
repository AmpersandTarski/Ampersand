<?

require "behandelaar.inc.php";
require "localsettings.inc.php";

// meta information of behandelaar
$object = new object("behandelaar",array
                      (new oRef( new oMulti( true,false,false,false ) // derived from assigned~
                               , new object("aanvragen",array(),"application.php")
                               )
                      ),"behandelaar.php");


$view = new view();
$view->assign("objname","behandelaar");
$obj=false;
$obj=parseRequest($object);

// do the requested action
if($action=='create'){
	$object_id=createBehandelaar($obj);
}else
if($action=='delete'){
	if(deleteBehandelaar($actionValue)){
		$view->assign("succes",true);
		$view->addOkMessage("Behandelaar deleted");
	}else{
		$object_id=$actionValue;
		$view->assign("succes",false);
	}
}else
if($action=='update'){
	updateBehandelaar($actionValue,$obj);
	$object_id=$actionValue;
	if($object_id===false) $view->assign("succes",false); else $view->assign("succes",true);
}else
if($action=='read'){
	if(isset($actionValue)){ // is not assigned for new!
		$object_id=$actionValue;
		$obj=readBehandelaar($object_id); // from DB
		if($object_id===false) $view->assign("succes",false);
	}
	if($obj===false) {
		echo 'empty';
		$obj=new behandelaar(null,array()); // return an empty object
	}
}

$view->assign("action",$action);


if($obj){ // read on no valid object id: send empty object
	// show the item itself
	$behandelaar=$obj; // from POST or something
	$header = new viewableText(@$object_id,'H2');
	$header->assign("caption",'behandelaar');
	$view->assign("header",$header);
	$list=new expandableList();
	$list->assign("object",$object);
	$list->assign("elements",array($behandelaar));
	$list->assign("One",true);
	if(isset($object_id)) $view->assign("object_id",$object_id);
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