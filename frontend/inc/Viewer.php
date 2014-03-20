<?php

class Viewer {
	
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
	
	public static function escapeHtmlAttrStr($str) {
		return str_replace(array('"', '&'), array('&quot;', '%26'), $str); // we do escapeSQL and replace \" by &quot; and \' by '
	}
	
}

?>