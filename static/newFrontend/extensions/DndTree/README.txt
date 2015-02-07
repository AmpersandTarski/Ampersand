// Define interface in ADL
INTERFACE "<interfacename>" FOR "Viewer" : '__MYSESSION__'
	BOX [ "children"	: V[SESSION*Project]
			BOX [ "children"	: (member \/ pl)
					BOX [ "children" 	: workswith]
				]
		]
		
// Add view-controller to $routeProvider in AmpersandApp.js
.when('/<interfacename>/:resourceId?',
	{
		controller: 'DndTreeController',
		templateUrl: 'extensions/DndTree/ui/views/DndTreeViewer.html'
	})

// Enable in localSettings
require_once(__DIR__ . '/extensions/DndTree/DndTree.php');

// Go!