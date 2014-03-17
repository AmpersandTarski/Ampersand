<?php
// This script keeps track of data too big for client side cookies.

session_start();

if (!isset($_POST['command'])) die();

$cmd = $_POST['command'];
$var = $_POST['variable'];

if ($cmd == 'store')
{
	$content = $_POST['content'];
	$_SESSION[$var] = $content;
}

if ($cmd == 'retrieve')
{
	if (!isset($_SESSION[$var])) die();
	echo $_SESSION[$var];
}
if ($cmd == 'delete')
{
	unset($_SESSION[$var]);
}	

?>
