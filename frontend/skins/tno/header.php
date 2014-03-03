<?php

?>
<div class="navbar navbar-default navbar-fixed-top">
	<div class="tno">
		<div class="container">
			<img src="images/tnotimeline.png" alt="TNO" />
		</div>
	</div>
	<div class="container">
		<ul class="nav nav-pills">
			<li id="overview">
				<a href="<?php echo ($selectedRoleNr>=0 ? '?role='.$selectedRoleNr : '');?>"><?php echo $dbName;?></a>
			</li>
				
			<?php
				topLevelInterfaceLinks();
			?>
			
			<li class="dropdown" class="pull-right">
				<a class="dropdown-toggle" data-toggle="dropdown" href="#"><span class="glyphicon glyphicon-plus"></span></a>
				<ul class="dropdown-menu" role="menu">
				  <?php  genNewAtomDropDownMenu();?>
				</ul>
			</li>
			
			<li class="dropdown" class="pull-right">
				<a class="dropdown-toggle" data-toggle="dropdown" href="#"><span class="glyphicon glyphicon-user"></span></a>
				<ul class="dropdown-menu" role="menu">
				  <?php  genRolesDropDownMenu();?>
				</ul>
			</li>
			
			<li id="viewer" class="pull-right">
				<a href="views/"><span class="glyphicon glyphicon-eye-open"></span></a>
			</li>
			
			<li id="MenuBarReset" class="pull-right">
				<a href="installer.php"><span class="glyphicon glyphicon-trash"></span></a>
			</li>

		</ul>
	</div>
</div>