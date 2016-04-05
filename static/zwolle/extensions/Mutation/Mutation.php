<?php

namespace Ampersand\Extension\Mutation;

use Ampersand\Hooks;
use Ampersand\Config;
use Ampersand\Database\Database;
use Ampersand\Core\Concept;
use Ampersand\Core\Relation;
use Ampersand\Session;
use Ampersand\Core\Atom;

// Define hooks
$updateHook = array('class' => '\Ampersand\Extension\Mutation\Mutation', 'function' => 'mutUpdate', 'filename' => 'Mutation.php', 'filepath' => 'extensions/Mutation', 
		'params' => array('$fullRelationSignature', '$stableAtom', '$stableConcept', '$modifiedAtom', '$modifiedConcept', '$source'));
$insertHook = array('class' => '\Ampersand\Extension\Mutation\Mutation', 'function' => 'mutInsert', 'filename' => 'Mutation.php', 'filepath' => 'extensions/Mutation',
		'params' => array('$fullRelationSignature', '$stableAtom', '$stableConcept', '$modifiedAtom', '$modifiedConcept', '$source'));
$deleteHook = array('class' => '\Ampersand\Extension\Mutation\Mutation', 'function' => 'mutDelete', 'filename' => 'Mutation.php', 'filepath' => 'extensions/Mutation',
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
			
			$database = Database::singleton();
			$database->setTrackAffectedConjuncts(false); // Don't track affected conjuncts for Mutation concept and relations;
			
			// New Mutation
			$mutConcept = Concept::getConcept(Config::get('mutationConcepts', 'MutationExtension')[$fullRelationSignature]);
			$mut = $mutConcept->createNewAtom();
			
			// Add mut info
			Relation::getRelation('mutRelation', $mut->concept->name, 'Relation')->addLink($mut, new Atom($fullRelationSignature, 'Relation'), false, 'MutationExtension');
			Relation::getRelation('mutDateTime', $mut->concept->name, 'DateTime')->addLink($mut, new Atom(date(DATE_ISO8601), 'DateTime'), false, 'MutationExtension');
			
			if($source == 'User'){
			    $session = Session::singleton();
				$accountId = $session->getSessionAccountId();
			}else{
				$accountId = $source;
			}
			
			Relation::getRelation('mutBy', $mut->concept->name, 'Account')->addLink($mut, new Atom($accountId, 'Account'), false, 'MutationExtension');
			Relation::getRelation('mutOp', $mut->concept->name, 'Operation')->addLink($mut, new Atom($operation, 'Operation'), false, 'MutationExtension');
			// Relation::getRelation('mutReason', $mut->concept->name, 'MutationReason')->addLink($mut, new Atom('zomaar', 'MutationReason'), false, 'MutationExtension'); // TODO: get reason from somewhere
			Relation::getRelation('mutValue', $mut->concept->name, 'MutationValue')->addLink($mut, new Atom($modifiedAtom, 'MutationValue'), false, 'MutationExtension');
			Relation::getRelation('mutStable', $mut->concept->name, $stableConcept)->addLink($mut, new Atom($stableAtom, $stableConcept), false, 'MutationExtension');
			Relation::getRelation('mutPublish', $mut->concept->name, $mut->concept->name)->addLink($mut, $mut, false, 'MutationExtension');

	       $database->setTrackAffectedConjuncts(true); // Enable tracking of affected conjuncts again!!
		}	
	}
}

?>
