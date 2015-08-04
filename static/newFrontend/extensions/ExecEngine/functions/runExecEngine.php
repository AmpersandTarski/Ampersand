<?php
function RerunExecEngine($logText = 'run ExecEngine again after completion'){	
		
	Notifications::addLog('Rerun: ' . $logText, 'ExecEngine');
	ExecEngine::$doRun = true;
	
	return true;
}
?>