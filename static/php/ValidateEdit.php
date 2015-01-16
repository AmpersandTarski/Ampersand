<?php
// NOTE: when moving this file, also modify its path in ValidateEdit.hs

$isValidationSession = true; // causes Generics.php to define a temporary db name instead of the normal one.

require __DIR__.'/Database.php';

if (!isset($argv[1])) {
  echo 'ValidateEdit.php requires a parameter that contains the JSON array of edit commands to be executed in temporary db.';
  return;
}

$commandsJson = $argv[1];
//$commandsJson = '[{"dbCmd": "addToConcept", "atom": "Ax","concept": "A"}]';
echo '$commandsJson = \''.$commandsJson.'\'';
processCommandsJson($commandsJson);
?>
