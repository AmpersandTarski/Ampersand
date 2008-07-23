<?

/*********** file Aanvraag.adl on line 3-5:
 * OBJECT behandelaar Employee          -- 'Employee' zou betekenen: 'Employee = I[Employee]'
 *        aanvragen : assigned~
 * ENDOBJECT
 ***********/

function getobject_behandelaar(){
	return new object("behandelaar",array
                      (new oRef( new oMulti( true,false,false,false ) // derived from assigned~
                               , new object("aanvragen",array(),"application.php")
                               )
                      ),"behandelaar.php");
}

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
function getEachBehandelaar(){
	return DB_doquer('SELECT AttE_mployee FROM C4_E_mployee');
}
function createBehandelaar(behandelaar &$behandelaar){
	return updateBehandelaar($behandelaar,true);
}
function readBehandelaar($id){
	$ctx = DB_doquer('SELECT AttE_mployee FROM C4_E_mployee WHERE AttE_mployee=\''.addslashes($id).'\'');
	if(count($ctx)==0){
		return false;
	}
	$obj = new behandelaar($id,array());
	$ctx = DB_doquer('SELECT AttA_pplication FROM T4_assigned WHERE AttE_mployee=\''.addslashes($id).'\'');
	foreach($ctx as $i=>$v){
		$obj->addAanvragen(new behandelaar_aanvragen($v['AttA_pplication']));
	}
	return $obj;
}
function updateBehandelaar(behandelaar &$behandelaar,$new=false){
	global $DB_link,$DB_err,$DB_lastquer;
	$preErr= $new ? 'Cannot create new Employee: ':'Cannot update Employee: ';
	DB_doquer('START TRANSACTION');
	if($new) {
		if(!isset($behandelaar->id))
		{	// find a unique id for behandelaar
			$nextNum = DB_doquer('SELECT max(1+AttE_mployee) FROM C4_E_mployee GROUP BY \'1\'');
			$behandelaar->id = $nextNum[0][0];
		}
		if(DB_plainquer('INSERT INTO C4_E_mployee (AttE_mployee) VALUES (\''.addslashes($behandelaar->id).'\')',$errno)===false){
			$DB_err=$preErr.(($errno==1062) ? 'Employee \''.$behandelaar->id.'\' already exists' : 'Error '.$errno.' in query '.$DB_lastquer);
			DB_doquer('ROLLBACK');
			return false;
		}
	}else{
		// destroy old T4_assigned values
		$effected=DB_doquer('SELECT AttA_pplication FROM T4_assigned WHERE AttE_mployee=\''.addslashes($behandelaar->id).'\'');
		$arr=array();
		foreach($effected as $i=>$v){$arr[]='\''.addslashes($v[0]).'\'';}
		$aanvragen_str = join(',',$arr);
		DB_doquer('DELETE FROM T4_assigned WHERE AttE_mployee=\''.addslashes($behandelaar->id).'\'');
	}
	foreach($behandelaar->aanvragen as $i=>$aanvragen){
		if(!isset($aanvragen->id)){
			$nextNum = DB_doquer('SELECT max(1+AttA_pplication) FROM C3_A_pplication GROUP BY 1');
			$aanvragen->id = $nextNum[0][0];
		}else{
			// check UNI of T4_assigned (Rule10)
			$taken = DB_doquer('SELECT AttE_mployee FROM T4_assigned WHERE AttA_pplication=\''.addslashes($aanvragen->id).'\'');
			if(count($taken))
			{
				$DB_err=$preErr.'\''.$aanvragen->id.'\' of the assigned aanvragen is allready taken by the employee \''.$taken[0][0].'\'.';
				DB_doquer('ROLLBACK');
				return false;
			}
		}
		// the Application may exist allready, so IGNORE duplicates
		DB_doquer('INSERT IGNORE INTO C3_A_pplication (AttA_pplication) VALUES (\''.addslashes($aanvragen->id).'\')');
		DB_doquer('INSERT IGNORE INTO T4_assigned (AttE_mployee,AttA_pplication) VALUES (\''.addslashes($behandelaar->id).'\',\''.addslashes($aanvragen->id).'\')');
	}
	if(!$new && strlen($aanvragen_str))
		DB_doquer('DELETE FROM C3_A_pplication
		  WHERE AttA_pplication IN ('.$aanvragen_str.')
		  	AND NOT EXISTS
		  		(SELECT * FROM T4_assigned
		  		  WHERE C3_A_pplication.AttA_pplication=T4_assigned.AttA_pplication
		  		)
		  	AND NOT EXISTS
		  		(SELECT * FROM T2_applicant
		  		  WHERE C3_A_pplication.AttA_pplication=T2_applicant.AttA_pplication
		  		)
		  	AND NOT EXISTS
		  		(SELECT * FROM T3_checked
		  		  WHERE C3_A_pplication.AttA_pplication=T3_checked.AttA_pplication
		  		)
		  	AND NOT EXISTS
		  		(SELECT * FROM T6_kind
		  		  WHERE C3_A_pplication.AttA_pplication=T6_kind.AttA_pplication
		  		)
		  	AND NOT EXISTS
		  		(SELECT * FROM T10_leadsto
		  		  WHERE C3_A_pplication.AttA_pplication=T10_leadsto.AttA_pplication
		  		)
		  		');
	if(!checkRule2()){
		$DB_err=$preErr.'Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only.';
	}else if(!checkRule4())
	{
		$DB_err=$preErr.'Applications for permits are treated by authorized personnel only.';
	} else {
		DB_doquer('COMMIT');
		return $behandelaar->id;
	}
	DB_doquer('ROLLBACK');
	return false;
}
function deleteBehandelaar($id){
	global $DB_err;
	DB_doquer('START TRANSACTION');
	
	$taken = DB_doquer('SELECT AttP_roduct FROM T5_auth WHERE AttE_mployee=\''.addslashes($id).'\'');
	if(count($taken))
	{
		$DB_err = 'Cannot delete behandelaar: Employee \''.addslashes($id).'\' is authorized to treat product \''.addslashes($taken[0][0]).'\'';
		DB_doquer('ROLLBACK');
		return false;
	}
	$taken = DB_doquer('SELECT AttA_rea FROM T9_area WHERE AttE_mployee=\''.addslashes($id).'\'');
	if(count($taken))
	{
		$DB_err = 'Cannot delete behandelaar: Employee \''.addslashes($id).'\' treats applications from area \''.addslashes($taken[0][0]).'\'';
		DB_doquer('ROLLBACK');
		return false;
	}
	
	DB_doquer('DELETE FROM C3_A_pplication
		  WHERE EXISTS
		        (SELECT * FROM T4_assigned
		          WHERE C3_A_pplication.AttA_pplication=T4_assigned.AttA_pplication
		            AND T4_assigned.AttE_mployee=\''.addslashes($id).'\'
		        )
		  	AND NOT EXISTS
		  		(SELECT * FROM T4_assigned
		  		  WHERE C3_A_pplication.AttA_pplication=T4_assigned.AttA_pplication
		  		    AND T4_assigned.AttE_mployee<>\''.addslashes($id).'\'
		  		)
		  	AND NOT EXISTS
		  		(SELECT * FROM T2_applicant
		  		  WHERE C3_A_pplication.AttA_pplication=T2_applicant.AttA_pplication
		  		)
		  	AND NOT EXISTS
		  		(SELECT * FROM T3_checked
		  		  WHERE C3_A_pplication.AttA_pplication=T3_checked.AttA_pplication
		  		)
		  	AND NOT EXISTS
		  		(SELECT * FROM T6_kind
		  		  WHERE C3_A_pplication.AttA_pplication=T6_kind.AttA_pplication
		  		)
		  	AND NOT EXISTS
		  		(SELECT * FROM T10_leadsto
		  		  WHERE C3_A_pplication.AttA_pplication=T10_leadsto.AttA_pplication
		  		)
		  		');
	DB_doquer('DELETE FROM C4_E_mployee WHERE AttE_mployee=\''.addslashes($id).'\'');
	DB_doquer('DELETE FROM T4_assigned WHERE AttE_mployee=\''.addslashes($id).'\'');
	
	if(!checkRule2()){ // example.. this one need not be here (for delete)
		$DB_err='Cannot delete employee: Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only.';
	}else if(!checkRule4()){ // example.. this one need not be here (for delete)
		$DB_err='Cannot delete employee: Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only.';
	}else{
		DB_doquer('COMMIT');
		return true;
	}
	DB_doquer('ROLLBACK');
	return false;
}

?>