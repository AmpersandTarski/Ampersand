<?php

// Define hooks
$GLOBALS['hooks']['after_Database_editUpdate_UPDATE'][] = 'Mutation::mutUpdate';
$GLOBALS['hooks']['after_Database_editUpdate_INSERT'][] = 'Mutation::mutInsert';
$GLOBALS['hooks']['after_Database_editDelete_UPDATE'][] = 'Mutation::mutDelete'; // mutDelete is correct!
$GLOBALS['hooks']['after_Database_editDelete_DELETE'][] = 'Mutation::mutDelete';

class Mutation {
	
	public static function mutUpdate($fullRelationSignature, $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept, $source){
		$operation = 'Changed';
		Mutation::saveMutation($operation, $fullRelationSignature, $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept, $source);
	}
	
	public static function mutInsert($fullRelationSignature, $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept, $source){
		$operation = 'Added';
		Mutation::saveMutation($operation, $fullRelationSignature, $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept, $source);
	}
	
	public static function mutDelete($fullRelationSignature, $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept, $source){
		$operation = 'Removed';
		Mutation::saveMutation($operation, $fullRelationSignature, $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept, $source);
	}
	
	private static function saveMutation($operation, $fullRelationSignature, $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept, $source){
		if(array_key_exists($fullRelationSignature, Config::get('mutationConcepts', 'MutationExtension'))){
			Notifications::addLog("Save mutation on '$fullRelationSignature' (editUpdate)", 'Mutation');
			
			$mutConcept = Config::get('mutationConcepts', 'MutationExtension')[$fullRelationSignature];
			$database = Database::singleton();
			
			// New Mutation
			$mut = $database->addAtomToConcept(Concept::createNewAtom($mutConcept), $mutConcept);
			
			// Add mut info
			$database->editUpdate('mutRelation', false, $mut, 'Mutation', $fullRelationSignature, 'Relation');
			$database->editUpdate('mutDateTime', false, $mut, 'Mutation', date(DATE_ISO8601), 'DateTime');
			
			if($source == 'User'){
				$user = Session::getSessionUserId();
			}else{
				$user = $source;
			}
			
			$database->editUpdate('mutBy', false, $mut, 'Mutation', $user, 'User');
			$database->editUpdate('mutOp', false, $mut, 'Mutation', $operation, 'Operation');
			// $database->editUpdate('mutReason', false, $mut, 'Mutation', 'zomaar', 'MutationReason'); // TODO: get reason from somewhere
			$database->editUpdate('mutValue', false, $mut, 'Mutation', $modifiedAtom, 'MutationValue');
			$database->editUpdate('mutStable', false, $mut, $mutConcept, $stableAtom, $stableConcept);
			$database->editUpdate('mutPublish', false, $mut, 'Mutation', $mut, 'Mutation');
			
		}	
	}
}

Notifications::addLog('Mutation extensions included', 'Mutation');

?>
