<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Strict//EN">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>

<meta http-equiv="Pragma" content="no-cache">
<meta http-equiv="no-cache">
<meta http-equiv="Expires" content="-1">
<meta http-equiv="cache-Control" content="no-cache">

</html>
<body>
<?php
  require 'InstallerDBstruct.php';
  require 'InstallerDefPop.php';
  require 'DumpPopulationToADL.php';
  
  if ($error==false) {
    echo '<div id="ResetSuccess"/>The database has been reset to its initial population.<br/><br/><button onclick="window.location.href = document.referrer;">Ok</button>';
  }
?>
</body>
</html>
