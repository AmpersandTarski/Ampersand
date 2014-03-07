<?php

require_once (__DIR__.'/../Generics.php'); // loading the Ampersand model

class Viewer {
	
	// TODO: make non static or private function
	public static function viewInterface($interface){
		
	
	}
	
	public static function getView($concept){
		global $allViews;
		
		foreach ((array)$allViews as $view){
			if ($concept == $view['concept'] || in_array($concept, Concept::getSpecializations($view['concept']))) return $view;
		}
		return null;
	}
	
	// TODO: make non static or private function
	public static function viewAtom($atom, $srcConcept){
		global $allViews; // from Generics.php
		$database = Database::singleton();
		
		foreach ($allViews as $view){
			if($view['concept'] == $srcConcept){
				$viewStrs = array ();
				foreach ($view['segments'] as $viewSegment) 
					if ($viewSegment['segmentType'] == 'Text')
						$viewStrs[] = htmlSpecialChars($viewSegment['Text']);
					elseif ($viewSegment['segmentType'] == 'Html')
						$viewStrs[] = $viewSegment['Html'];
					else {
						$query = "SELECT DISTINCT `tgt` FROM (".$viewSegment['expSQL'].") AS results WHERE src='".addslashes($atom)."'";
						$rows = $database->Exe($query);
						$txt = count($rows) ? $rows[0]['tgt'] : "<View relation not total>"; // this can happen in a create-new interface when the view fields have not yet been filled out.
						$viewStrs[] = htmlSpecialChars($txt);
					}
				return implode($viewStrs);
			}		
		}
		return $atom; // in case no view exists for this $srcConcept
	}

}

?>