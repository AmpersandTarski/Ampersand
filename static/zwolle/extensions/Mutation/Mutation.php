<?php

// Define hooks
$updateHook = array('class' => 'Mutation', 'function' => 'mutUpdate', 'filename' => 'Mutation.php', 'filepath' => 'extensions/Mutation', 
		'params' => array('$fullRelationSignature', '$stableAtom', '$stableConcept', '$modifiedAtom', '$modifiedConcept', '$source'));
$insertHook = array('class' => 'Mutation', 'function' => 'mutInsert', 'filename' => 'Mutation.php', 'filepath' => 'extensions/Mutation',
		'params' => array('$fullRelationSignature', '$stableAtom', '$stableConcept', '$modifiedAtom', '$modifiedConcept', '$source'));
$deleteHook = array('class' => 'Mutation', 'function' => 'mutDelete', 'filename' => 'Mutation.php', 'filepath' => 'extensions/Mutation',
		'params' => array('$fullRelationSignature', '$stableAtom', '$stableConcept', '$modifiedAtom', '$modifiedConcept', '$source'));
Hooks::addHook('postDatabaseUpdate', $updateHook);
Hooks::addHook('postDatabaseInsert', $insertHook);
Hooks::addHook('postDatabaseDelete', $deleteHook);

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
			
			$database = Database::singleton();
			$database->setTrackAffectedConjuncts(false); // Don't track affected conjuncts for Mutation concept and relations;
			
			// New Mutation
			$mutConcept = Config::get('mutationConcepts', 'MutationExtension')[$fullRelationSignature];
			$database->addAtomToConcept($mut = Concept::createNewAtom($mutConcept));
			
			// Add mut info
			$database->editUpdate(Relation::getRelation('mutRelation', $mut->concept->name, 'Relation'), false, $mut, new Atom($fullRelationSignature, 'Relation'));
			$database->editUpdate(Relation::getRelation('mutDateTime', $mut->concept->name, 'DateTime'), false, $mut, new Atom(date(DATE_ISO8601), 'DateTime'));
			
			if($source == 'User'){
				$account = Session::getSessionAccountId();
			}else{
				$account = $source;
			}
			
			$database->editUpdate(Relation::getRelation('mutBy', $mut->concept->name, 'Account'), false, $mut, new Atom($account, 'Account'));
			$database->editUpdate(Relation::getRelation('mutOp', $mut->concept->name, 'Operation'), false, $mut, new Atom($operation, 'Operation'));
			// $database->editUpdate(Relation::getRelation('mutReason', $mut->concept->name, 'MutationReason'), false, $mut, new Atom('zomaar', 'MutationReason')); // TODO: get reason from somewhere
			$database->editUpdate(Relation::getRelation('mutValue', $mut->concept->name, 'MutationValue'), false, $mut, new Atom($modifiedAtom, 'MutationValue'));
			$database->editUpdate(Relation::getRelation('mutStable', $mut->concept->name, $stableConcept), false, $mut, new Atom($stableAtom, $stableConcept));
			$database->editUpdate(Relation::getRelation('mutPublish', $mut->concept->name, $mut->concept->name), false, $mut, $mut);

	       $database->setTrackAffectedConjuncts(true); // Enable tracking of affected conjuncts again!!
		}	
	}
}

Notifications::addLog('Mutation extensions included', 'Mutation');

?>
