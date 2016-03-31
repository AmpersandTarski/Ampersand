<?php
function RerunExecEngine($logText = 'run ExecEngine again after completion'){	
		
	\Ampersand\Logger::getLogger('EXECENGINE')->debug("Rerun: {$logText}");
	ExecEngine::$doRun = true;
	
	return true;
}
?>