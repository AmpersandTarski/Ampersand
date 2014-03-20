<?php

$viewers['AmpersandViewer'] = array('name' => 'Ampersand viewer', 'class' => 'AmpersandViewer', 'icon' => ''); // activeer viewer extension in framework

class AmpersandViewer extends Viewer {
	
	private $interface;
	private $atomId;
	
	public function __construct($interfaceName, $atomId = null){
		
		$this->interface = new UserInterface($interfaceName);
		$this->atomId = $atomId;
		
	}
	
	public function getView(){ 
		$db = Database::singleton();
		$session = Session::singleton();
		
		global $allInterfaceObjects;
		global $autoRefreshInterval;
		global $isDev;
		
		if (!isset($this->atomId)) {

			// Add dummy AmpersandRoot with just the refresh interval and timestamp to auto update signals.
			// This will be obsolete once these and other properties are in a separate div. 
			$this->emit($html, "<div id=AmpersandRoot refresh=$autoRefreshInterval timestamp=\"".$db->getLatestUpdateTime()."\"></div>"); 

		} else {
			$interface = $this->interface->name;

			$concept = $allInterfaceObjects[$interface]['srcConcept'];

			$isNew = ($concept != 'ONE' and !Concept::isAtomInConcept($this->atomId, $concept)); // $isNew boolean.

			$this->emit($html, '<div id=AmpersandRoot interface="'.Viewer::escapeHtmlAttrStr($interface).'" atom="'.Viewer::escapeHtmlAttrStr($this->atomId)
			.'" concept="'.Viewer::escapeHtmlAttrStr($allInterfaceObjects[$interface]['srcConcept'])
			.'" editing='.var_export($isNew, true).' isNew='.var_export($isNew,true)
			." refresh=$autoRefreshInterval dev=".($isDev ? 'true' : 'false').' role="'.Viewer::escapeHtmlAttrStr($session->role->name)
			.'" timestamp="'.$db->getLatestUpdateTime.'">');


			if  (!empty($allInterfaceObjects[$interface]['editableConcepts'])){ // 
				$this->emit($html, '<div class="container">');
				$this->emit($html, '<button class="Button EditButton" onclick="startEditing()">Edit</button>');
				$this->emit($html, '<button class="Button SaveButton" onclick="commitEditing()">Save</button>');
				$this->emit($html, '<button class="Button CancelButton" onclick="cancelEditing()">Cancel</button>');
				$this->emit($html, '</div>');
			}

			// If the atom is not in the concept, this means that a new atom was be created (and $this->atomId is a time-based unique name).
			// We cannot use a url-encoded command for Create new, since such a url causes problems in the browser history. (pressing back
			// could cause the creation of another atom) With the current method, going back or refreshing the url simply shows the new atom.
			// TODO: Once navigation is not done with urls anymore, we can employ a more elegant solution here.
			//
			// We add the atom to its concept in a temporary transaction, so we can generate the interface in the normal way (by querying
			// the database). When the interface is done, the transaction is rolled back. On save, the atom is added to the concept table
			// again.
			// TODO: with multiple users, this mechanism may lead to non-unique new atom names, until we enocode a session number
			//       in the unique atom name. But since the atom names are based on microseconds, the chances of a problem are pretty slim.
			if ($isNew){
				$db->Exe("START TRANSACTION"); // TODO: vervangen door database transaction function
				$db->addAtomToConcept($this->atomId, $concept);
			}

			// we need an extra ScrollPane div because the log windows need to be outside scroll area but inside ampersand root
			// (since their css depends on the 'editing' attribute)
			$this->emit($html, '<div id=ScrollPane>');
			$this->emit($html, $this->generateAtomInterfaces($allInterfaceObjects[$interface], $this->atomId, true)); 
			$this->emit($html, '</div>');

			$this->emit($html, '</div>');
			$this->emit($html, '<div id=Rollback></div>'); // needs to be outside AmpersandRoot, so it's easy to address all interface elements not in the Rollback

			if ($isNew) {
				$db->Exe("ROLLBACK"); // TODO: vervangen door database transaction function
			}
		} 
		
		// add some javascript functions needed by Ampersand.js
		$this->emit($html, '<script type="text/javascript">');
		
		$this->emit($html, $this->generateInterfaceMap());
		$this->emit($html, $this->genEditableConceptInfo($this->interface->name));
		$this->emit($html, 'function getSelectedRole(){ return '.$session->role->id.';}');
		$this->emit($html, '</script>');

		return $html;
	
	}
	
	private function generateInterface($interface, $srcAtom, $isRef=false) {
		/*
		*  <Interface label='interface label' isRef=true'/'false'>
		*   <Label>interface label</Label>
		*   <AtomList concept=.. [relation=..  relationIsFlipped=..]>
		*     ..
		*     for each $tgtAtom in codomain of relation of $interface
		*     <AtomRow rowType=Normal>         <DeleteStub/> <AtomListElt> generateAtomInterfaces($interface, $tgtAtom) </AtomListElt> </AtomRow>
		*     ..
		*     
		*     <AtomRow rowType=NewAtomTemplate> <DeleteStub/> <AtomListElt> generateAtomInterfaces($interface, null) </AtomListElt>     </AtomRow>
		*     
		*     <AtomRow rowType=InsertAtomStub> <DeleteStub/> <InsertStub>Insert new .. </InsertStub>                                  </AtomRow>
		*   </AtomList>
		* </Interface> 
		*/

		$database = Database::singleton();

		$html = "";

		$this->emit($html, '<div class=Interface label="'.Viewer::escapeHtmlAttrStr($interface['name']).'" isRef="'.var_export($isRef, true).'">');
		$this->emit($html, "<div class=Label>".htmlSpecialChars($interface['name']).'</div>');

		if ($srcAtom == null){
			$codomainAtoms = array (); // in case the table would contain (null, some atom)  
		}else{

			$codomainAtoms = array_column($database->Exe("SELECT DISTINCT `tgt` FROM (".$interface['expressionSQL'].") AS results WHERE src='".addslashes($srcAtom)."' AND `tgt` IS NOT NULL"), 'tgt'); // IS NOT NULL to filter for contains ($srcAtom, null)
		}

		if (count($codomainAtoms)==0 && isset($interface['min']) && $interface['min']=='One'){ // 'min' is only defined for editable relations  
			$codomainAtoms[] = ""; // if there should be at least one field, we add an empty field.
		}

		$codomainAtoms[] = null; // the null is presented as a NewAtomTemplate (which is cloned when inserting a new atom)

		$nrOfAtoms = count($codomainAtoms)-1; // disregard the null for the NewAtomTemplate

		$relationAttrs = $interface['relation']=='' ? '' : ' relation="'.Viewer::escapeHtmlAttrStr($interface['relation']).'" relationIsFlipped="'.var_export($interface['relationIsFlipped'], true)
							.'" min="'.Viewer::escapeHtmlAttrStr($interface['min']).'" max="'.Viewer::escapeHtmlAttrStr($interface['max'])
							.'" nrOfAtoms="'.Viewer::escapeHtmlAttrStr($nrOfAtoms).'"'; // 

		$this->emit($html, '<div class="AtomList" concept="'.Viewer::escapeHtmlAttrStr($interface['tgtConcept']).'"'.$relationAttrs.'>');

		foreach($codomainAtoms as $i => $tgtAtom) { // null is the NewAtomTemplate
			$this->emit($html, '<div class=AtomRow rowType='.($tgtAtom===null ?'NewAtomTemplate': 'Normal').'><div class=DeleteStub>&nbsp;</div>'.'<div class=AtomListElt>');
			$this->emit($html, $this->generateAtomInterfaces($interface, $tgtAtom));
			$this->emit($html,'</div></div>');  
		}

		$this->emit($html, '<div class=AtomRow rowType=InsertAtomRow><div class=DeleteStub>&nbsp;</div>'.'<div class=InsertStub>Insert new '.htmlSpecialChars($interface['tgtConcept']).'</div></div>');
		$this->emit($html, '</div></div>'); // close .AtomList and .Interface
		
		return $html;
	}

	private function generateAtomInterfaces($interface, $atom, $isTopLevelInterface=false) {
		/* if $interface is a top-level interface, we only generate for $interface itself
		* otherwise, we generate for its subinterfaces 
		* 
		*  <Atom atom='atom name'>
		*   <AtomName>atom name</AtomName>
		*   <InterfaceList>
		*     ..
		*     for each subInterface in $interface: generateInterface($interface, $atom)        (or $interface, if $isTopLevelInterface)
		*     ..
		*   </InterfaceList>
		* </Atom>
		* 
		* if $atom is null, we are presenting a template. Because ""==null and "" denotes an empty atom, we check with === (since "" !== null)
		*/
		global $session;
		global $allInterfaceObjects;


		$html = "";
		$subInterfaceIsRef = false;

		if ($isTopLevelInterface){
			$subInterfaces = array ($interface);
		}else{
			if (isset($interface['boxSubInterfaces'])){
				$subInterfaces = $interface['boxSubInterfaces'];
			}else{ 
				if (isset($interface['refSubInterface'])) {
					$subInterfaces = array ($allInterfaceObjects[$interface['refSubInterface']]);
					$subInterfaceIsRef = true;
				}else{
					$subInterfaces = array ();
				}
			}
		}

		// note that the assignments below are about interfaces for the atom, not about the subinterfaces 
		$nrOfInterfaces = count($session->role->getInterfaces(null, $interface['tgtConcept']));
		$hasInterfaces = $nrOfInterfaces == 0 ? '' : ' hasInterface=' . ($nrOfInterfaces == 1 ? 'single' : 'multiple');

		$this->emit($html, 	'<div class=Atom atom="'.Viewer::escapeHtmlAttrStr($atom).'"'.$hasInterfaces.
							' status='.($atom!==null?'unchanged':'new').
							' atomic="'.var_export(count($subInterfaces)==0, true).'">');

		$atomName = Viewer::viewAtom($atom, $interface['tgtConcept']); 

		// TODO: can be done more efficiently if we query the concept atoms once for each concept

		$this->emit($html, "<div class=AtomName>".$atomName.'</div>');
		
		if (count($subInterfaces) > 0) {
			$this->emit($html, '<div class=InterfaceList>');
			foreach($subInterfaces as $interface) {
				$this->emit($html, $this->generateInterface($interface, $atom, $subInterfaceIsRef));
			}
			$this->emit($html, '</div>'); // div class=InterfaceList
		}
		
		$this->emit($html, '</div>'); // div class=Atom
		
		return $html;
	}
	
	private function genEditableConceptInfo($interfaceName) {
		$atomViewMap = array ();
		$interface = new UserInterface($interfaceName);
		
		foreach ($interface->editableConcepts as $editableConcept) {
			$atomsAndViews = array ();
			
			foreach (Concept::getAllAtoms($editableConcept) as $atom) {
				$atomsAndViews[] = array ('atom' => $atom, 'view' => Viewer::viewAtom($atom, $editableConcept));
			}
			$atomViewMap[$editableConcept] = array ('hasView' => Concept::getView($editableConcept)!=null, 'atomViewMap' => $atomsAndViews);
		}
		
		$atomViewMapJson = json_encode( $atomViewMap );
		
		return "function getEditableConceptInfo() { return ".$atomViewMapJson."; }";
	}
	
	// TODO: can be deleted when not using Ampersand.js anymore
	private function generateInterfaceMap(){
		$session = Session::singleton();
		
		$interfaceMap = 'function getInterfacesMap() { var interfacesMap = new Array();'; // TODO: use Json for this
		foreach($session->role->getInterfaces() as $interface) {
				$conceptOrSpecs = array_merge(array($interface->srcConcept), Concept::getSpecializations($interface->srcConcept));

				foreach ($conceptOrSpecs as $concept) 
				$interfaceMap .= '  mapInsert(interfacesMap, "'.Viewer::escapeHtmlAttrStr($concept).'", "'.Viewer::escapeHtmlAttrStr($interface->name).'");';
		}
		$interfaceMap .= '  return interfacesMap; }';
		
		return $interfaceMap;
	}

	private function emit(&$lines,$line) {
		$lines.=$line."\n";
	}


}

?>