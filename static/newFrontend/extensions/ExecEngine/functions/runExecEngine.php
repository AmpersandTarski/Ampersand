<?php
function RerunExecEngine(){	
		
	Notifications::addLog("Request ExecEngine to run again after completion", 'ExecEngine');
	ExecEngine::$doRun = true;
	
	return true;
}
?>