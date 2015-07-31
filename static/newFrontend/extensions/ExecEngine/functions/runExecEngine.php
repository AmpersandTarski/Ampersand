<?php
function RerunExecEngine(){	
		
	Notifications::addLog("Request ExecEngine to run again after completion");
	ExecEngine::$doRun = true;
	
	return true;
}
?>