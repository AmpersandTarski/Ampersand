<?php
if (!isset($_SERVER['AUTH_USER'])|| $_SERVER['AUTH_USER']=='') {
  if (!isset($_SERVER['PHP_AUTH_USER'])) {
    header('WWW-Authenticate: Basic realm="Ampersand - Bedrijfsregels"');
    echo 'Just enter a name without password. Refresh the page to retry...';
    exit;
  } else {
    DEFINE("USER",str_replace(" ","",$_SERVER['PHP_AUTH_USER']));
  }
} else {
  DEFINE("USER", str_replace("\\", "_", $_SERVER['AUTH_USER']));
}

DEFINE("COMPILATIONS_PATH","comp/".USER."/");
@mkdir(COMPILATIONS_PATH);
DEFINE("FILEPATH","comp/".USER."/uploads/");
@mkdir(FILEPATH);

$fullfile = 'empty.adl';
if (isset($_REQUEST['file'])) {$fullfile = $_REQUEST['file'];}
if(isset($_POST['adlbestand'])){
        $tmp_name = $_FILES["uploadfile"]["tmp_name"];
        $i=1;
        $name = str_replace(".adl","",$_FILES["uploadfile"]["name"]).'.v'.$i.'.adl';
        while (file_exists(FILEPATH.$name)){
           $j=$i;
           $i++;
           $name = str_replace(".v".$j.".",".v".$i.".",$name);
        }
        move_uploaded_file($tmp_name, FILEPATH.$name);
        $fullfile = FILEPATH.$name;
}
if(isset($_POST['adllaad'])){
        $fullfile = FILEPATH.$_POST['filename'];
}
if(isset($_POST['adltekst']) || $_REQUEST['operation']==3 || $_REQUEST['operation']==2){
   $dtstr = '.'.strftime('%Y%m%d%H%M%S').'.';
   $oldfile = basename($_REQUEST['file']);
   if (isset($_POST['filename'])) {$oldfile = $_POST['filename'];} 
   if (preg_match("/\.\d{14}\./", $oldfile)==0)
       {echo 'x'; $fullfile = FILEPATH.str_replace(".adl","",$oldfile).$dtstr."adl";}
   else
       $fullfile = FILEPATH.preg_replace("/\.\d{14}\./",$dtstr, $oldfile);
}
$file = basename($fullfile);

DEFINE("LOGPATH","comp/".USER."/log/");
@mkdir(LOGPATH);
//$target = escapeshellcmd(COMPILATIONS_PATH.'log/');
//$source = escapeshellcmd(FILEPATH.$file->getId().'.adl');
//if(!is_dir($target)) mkdir($target) or exit('error:could not create directory '.$target);
unlink(LOGPATH.'error.txt');
unlink(LOGPATH.'verbose.txt');
$descriptorspec = array(
      0 => array("pipe", "r"),  // stdin is a pipe that the child will read from
      //1 => array("pipe", "w"),  // stdout is a pipe that the child will write to
      1 => array("file", LOGPATH."verbose.txt", "a"),  // stdout is a pipe that the child will write to
      //2 => array("pipe", "w") // stderr is a pipe that the child will write to
      2 => array("file", LOGPATH."error.txt", "a") // stderr is a pipe that the child will write to
  //    2 => array("file", "/error-output.txt" ,"a") // stderr is a file to write to
      );
//echo getcwd();


$str = array(
     1 => './adl --verbose -p'.COMPILATIONS_PATH.' --theme=student --import="'.$fullfile.'" --namespace='.USER.' --importformat=adl atlas',
     2 => './adl --verbose -o'.FILEPATH.' --export='.$file.' --namespace='.USER.' atlas',
     3 => './adl --verbose -o'.FILEPATH.' --export='.$file.' --namespace='.USER.' atlas',
     4 => './amp --verbose -o'.COMPILATIONS_PATH.' -f Latex "'.$fullfile.'"',
     5 => './adl --verbose -p'.COMPILATIONS_PATH.'proto/ --theme=student "'.$fullfile.'"',
     99 => './adl --verbose "'.$fullfile.'"'
     );
$compileurl = array(
     1 => COMPILATIONS_PATH.'ctxAtlas.php?content=Meterkast&Meterkast='.USER,
     2 => 'index.php?file='.$fullfile,
     3 => 'index.php?file='.$fullfile.'&operation=1',
     4 => COMPILATIONS_PATH.$_REQUEST['output'],
     5 => COMPILATIONS_PATH.'proto/index.htm',
     99 => ''
     );

if (isset($_REQUEST['operation']) || isset($_POST['adltekst']) || isset($_POST['adlbestand']) || isset ($_POST['adllaad'])){
   if (isset($_POST['adllaad'])|| isset($_POST['adlbestand'])) {$operation = 1;}
   elseif (isset($_POST['adltekst'])) {
          $operation = 1;
          file_put_contents($fullfile,$_POST['adltext']);
   } else $operation = $_REQUEST['operation'];

   $process = proc_open($str[$operation], $descriptorspec, $pipes, getcwd());
   if (is_resource($process)) {
        fclose($pipes[0]);
       //fclose($pipes[2]);
       //$perr = stream_get_contents($pipes[2]);
       //$pout = stream_get_contents($pipes[1]);
       //fclose($pipes[1]);
       //fclose($pipes[2]);
       // It is important that you close any pipe before calling
       // proc_close in order to avoid a deadlock
       $return_value = proc_close($process);
   }

   if (file_exists(LOGPATH.'error.txt')){
       foreach( file ( escapeshellcmd(LOGPATH.'error.txt')) as $line)
              {$errorlns = $errorlns.'<p>'.$line.'</p>'; }
   }
   if (file_exists(LOGPATH.'verbose.txt'))
       foreach( file ( escapeshellcmd(LOGPATH.'verbose.txt')) as $line)
              {$verboselns = $verboselns.'<p>'.$line.'</p>'; }
} //end running the operation
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/strict.dtd">
<HTML>
<HEAD>
<?php
if (isset($operation) && $compileurl[$operation]!='' && !isset($errorlns))
   echo '<meta HTTP-EQUIV="REFRESH" content="0; url='.$compileurl[$operation].'">';
if (isset($_REQUEST['adlhuidige']))
   echo '<meta HTTP-EQUIV="REFRESH" content="0; url='.$compileurl[1].'">';
if (isset($_REQUEST['logout']))
   echo '<meta HTTP-EQUIV="REFRESH" content="0; url=logout.htm">';
?>
<TITLE>Ampersand</TITLE>
<SCRIPT type="text/javascript" src="overlib421/overlib.js"><!-- overLIB (c) Erik Bosrup --></SCRIPT>
<link rel="stylesheet" type="text/css" href="style.css" />
</HEAD>
<BODY>
  <div id="overDiv" style="position:absolute; visibility:hidden; z-index:1000;"></div><!--needed for overLIB-->

<p><div style="width:100%;background-color:#ffffff;margin:0px;padding:0px;top:0px"> <img style="margin:0px;padding:0px" src="ou.jpg">
</div></p>
<?php
if (isset($operation)){
   if ($compileurl[$operation]!='' && !isset($errorlns))
      echo '<A HREF="'.$compileurl[$operation].'">klik hier om naar de output te gaan.</A>';
   echo '<p>'.$str[$operation].'</p>';
   echo $verboselns;
   echo $errorlns;
}else{
   echo '<FORM name="myForm" action="'.$_SERVER['PHP_SELF'].'" method="POST" enctype="multipart/form-data">';
   echo '<H1>Laad de context in de Atlas...</H1>';

   echo '<p><input type="submit" name="adlhuidige" value="... die '.USER.' het laatst geladen heeft"/>';
   echo '<a href="javascript:void(0);"';
   echo 'onmouseover="return overlib(\'<p>Let op! Eventuele wijzigingen in het tekstveld gaan verloren!</p>\',WIDTH, 350);"';
   echo 'onmouseout="return nd();">';
   echo '<IMG SRC="warning.png" /></a></p>';

   echo '<p><input type="submit" name="adlbestand" value="... uit bestand op uw computer" />';
   echo '<a href="javascript:void(0);"';
   echo 'onmouseover="return overlib(\'<p>Let op! Eventuele wijzigingen in het tekstveld gaan verloren!</p><p>Let op! Eventuele niet gecommitte wijzigingen in de laatst geladen context in de Atlas gaan verloren!</p>\',WIDTH, 350);"';
   echo 'onmouseout="return nd();">';
   echo '<IMG SRC="warning.png" /></a>';
   echo '<input type="file" name="uploadfile" /></p>';

   if (!isset($_REQUEST['browse'])){     
     echo '<p><input type="submit" name="adltekst" value="... uit onderstaand tekstveld (save)"/>';
     echo '<a href="javascript:void(0);"';
     echo 'onmouseover="return overlib(\'<p>Let op! Eventuele niet gecommitte wijzigingen in de laatst geladen context in de Atlas gaan verloren!</p>\',WIDTH, 350);"';
     echo 'onmouseout="return nd();">';
     echo '<IMG SRC="warning.png" /></a></p>';

     echo '<p><input type="submit" name="adllaad" value="... uit in tekstveld geladen versie"/>';
     echo '<a href="javascript:void(0);"';
     echo 'onmouseover="return overlib(\'<p>Let op! Eventuele wijzigingen in het tekstveld gaan verloren!</p><p>Let op! Eventuele niet gecommitte wijzigingen in de laatst geladen context in de Atlas gaan verloren!</p>\',WIDTH, 350);"';
     echo 'onmouseout="return nd();">';
     echo '<IMG SRC="warning.png" /></a>';
     echo '<a href="javascript:void(0);"';
     echo 'onmouseover="return overlib(\'<p>De laadoptie &quot;uit onderstaand tekstveld (save)&quot; creëert een nieuwe contextversie<p/><p>Als u geen wijzigingen in het tekstveld heeft gemaakt, of deze niet wil opslaan, gebruik dan deze laadoptie</p><p>De contextversie uit serverbestand '.$fullfile.' zal worden geladen.</p>\',WIDTH, 350);"';
     echo 'onmouseout="return nd();">';
     echo '<IMG SRC="info.png" /></a></p>';

     echo '<p>Naam van contextversie in tekstveld ';
     echo '<input type="text" name="filename" value="'.$file.'"/>';
     echo '<a href="javascript:void(0);"';
     echo 'onmouseover="return overlib(\'<p>U mag deze naam wijzigingen.</p><p>Als u laadoptie &quot;... uit onderstaand tekstveld (save)&quot; gebruikt, dan wordt de contextversie in het tekstveld onder deze naam in een serverbestand opgeslagen.</p><p>Een timestamp zal toegevoegd (of ververst) worden.</p>\',WIDTH, 350);"';
     echo 'onmouseout="return nd();">';
     echo '<IMG SRC="info.png" /></a></p>';
     ?>
     <P><textarea name="adltext" cols=100 rows=30><?php
     $i=0;
     foreach( file (escapeshellcmd($fullfile)) as $line){
       $i++;
       $line = (preg_replace('/{-(\d+)-}/','',$line));
       echo '{-'.$i.'-}'.$line;
     }?></textarea></P>
     <?php
   }
   echo '</FORM>';
}
?>
</BODY>
</HTML>

