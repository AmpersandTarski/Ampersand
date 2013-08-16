<?php
/* This file defines the functions 'SetAttr', 'InsPair' and 'DelPair'. 
We do not give any guarantees with respect to their 100% functioning.
The reason is that we have used 'editUpdate' and 'editDelete', which we suspect of being buggy
and also we're not 100% sure whether we call it correctly in all instances.
Have fun...
*/

// Use: VIOLATION (TXT "{EX} SetAttr;<rel>;<SrcConcept>;<srcAtom>;<TgtConcept>;<tgtAtom>")
function SetAttr($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)
{
  emitAmpersandErr("Set attribute ($srcAtom,$tgtAtom)");
// Under the condition that $relation is [UNI], this is the same as a regular Update.
  editUpdate($relation, false, $srcAtom, $tgtAtom, 'child', NULL);
}

/* Example of rule that automatically inserts pairs into a relation:
  ROLE ExecEngine MAINTAINS "New Customers"
  RULE "New Customers": customerOrder[Person*Order];companyOrder[Company*Order]~ |- customerOf[Person*Company]
  MEANING "If a person places an order at a company, the person is a customer of that company"
  VIOLATION (TXT "{EX} InsPair;customerOf;Person;", SRC I, TXT";Company;", TGT I)
*/

// Use:  VIOLATION (TXT "{EX} InsPair;<rel>;<SrcConcept>;<srcAtom>;<TgtConcept>;<tgtAtom>")
function InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)
{
  emitAmpersandErr ("Insert pair ($srcAtom,$tgtAtom) into $relation($srcConcept*$tgtConcept)");
// NOTE: if $originalAtom == '', editUpdate means insert; als-ie er wel is is het dus een Update
// function editUpdate($rel, $isFlipped, $parentAtom, $childAtom, $parentOrChild, $originalAtom) 
  editUpdate($relation, false, $srcAtom, $tgtAtom, 'child', NULL);
}

/*
Example of a rule that automatically deletes pairs from a relation:
  ROLE ExecEngine MAINTAINS "Remove Customers"
  RULE "Remove Customers": customerOf[Person*Company] |- customerOrder[Person*Order];companyOrder[Company*Order]~
  MEANING "Customers of a company for which no orders exist (any more), are no longer considered customers"
  VIOLATION (TXT "{EX} DelPair;customerOf;Person;", SRC I, TXT";Company;", TGT I)
*/

// Use: VIOLATION (TXT "{EX} DelPair;<rel>;<SrcConcept>;<srcAtom>;<TgtConcept>;<tgtAtom>")
function DelPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)
{
	emitAmpersandErr ( "Delete pair ($srcAtom,$tgtAtom) in $relation($srcConcept*$tgtConcept)");
  editDelete($relation, false, $srcAtom, $tgtAtom);
}
?>