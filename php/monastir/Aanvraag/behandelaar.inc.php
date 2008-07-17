<?

/*********** file Aanvraag.adl on line 3-5:
 * OBJECT behandelaar  I[Employee]          -- 'Employee' zou betekenen: 'Employee = I[Employee]'
 * WITH aanvragen : assigned~
 * ENDOBJECT
 ***********/

class behandelaar {
	var $aanvragen;
	var $id;
	function behandelaar($id=null, /* behandelaar_aanvragen */ array $aanvragen = array()){
		$this->id=$id;
		$this->aanvragen=$aanvragen;
	}
	function addAanvragen(behandelaar_aanvragen $aanvragen){
		return $this->aanvragen[]=$aanvragen;
	}
	function addGen($type,$value){
		if($type=='aanvragen') return $this->addAanvragen($value);
		else return false;
	}
}
class behandelaar_aanvragen {
	var $id;
	function behandelaar_aanvragen($id){
		$this->id=$id;
	}
}
function createBehandelaar(behandelaar $obj){
	echo 'create Behandelaar';
	return $id;
}
function readBehandelaar($id){
	$obj = new behandelaar($id,array());
	$ctx = DB_doquer('SELECT AttA_pplication FROM T4_assigned WHERE AttE_mployee=\''.addslashes($id).'\'');
	foreach($ctx as $i=>$v){
		$obj->addAanvragen(new behandelaar_aanvragen($v['AttA_pplication']));
	}
	return $obj;
}
function updateBehandelaar($id,behandelaar $obj){
	$success=false;
	echo 'update Behandelaar';
	return $success;
}
function deleteBehandelaar($id){
	$success=false;
	echo 'delete Behandelaar';
	return $success;
}

?>