<?php

use Ampersand\Log\Logger;
use Ampersand\Extension\ExecEngine\ExecEngine;

function RerunExecEngine($logText = 'run ExecEngine again after completion'){	
		
	Logger::getLogger('EXECENGINE')->debug("Rerun: {$logText}");
	ExecEngine::$doRun = true;
	
	return true;
}
?>