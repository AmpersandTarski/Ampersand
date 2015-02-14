// Define interface in ADL
INTERFACE "<interfacename>" FOR "Viewer" : '__MYSESSION__'
	BOX [ "children"	: V[SESSION*Rootconcept]
			BOX [ "children"	: expression[RootConcept*SomeOtherConcept])
					BOX [ "children" 	: expression[SomeOtherConcept*AnotherSomeConcept]
				]
		]
		
// Add view-controller to $routeProvider in file "<HTDOCSROOT>\<APPLICATIONDIR>\App\AmpersandApp.js"
.when('/<interfacename>/:resourceId?',
	{
		controller: 'DndTreeController',
		templateUrl: 'extensions/DndTree/ui/views/DndTreeViewer.html'
	})

// Enable in file "<HTDOCSROOT>\<APPLICATIONDIR>\localSettings.php":
require_once(__DIR__ . '/extensions/DndTree/DndTree.php');

// Go!