/*
 * BETA. Not needed in the generator for now.
 * 
 */


$scope.popUp = function(obj, property, ResourceId){
	var modalInstance = $modal.open({
		templateUrl		: 'generics/app/views/Project_addProjectleider.html',
		controller		: 'static_addModalController',
		size			: 'lg', 			// optional 'sm' (small), 'lg' (large)
		backdrop		: true,				// true, false or 'static'
		resolve			: { restUrl: function () { return 'interface/Person'; } }	// an optional map of dependencies which should be injected into the controller			
	});
	
	modalInstance.result // a promise that is resolved when a modal is closed and rejected when a modal is dismissed
		.then( // then() called when promise is resolved or rejected
			function (selectedId) { // function when modal is closed
				console.log('selected: ' + selectedId);
				selected = {id : selectedId};
				$scope.addObject(obj, property, selected, ResourceId);
			}, function () { // function when modal is dismissed
				console.log('Modal dismissed at: ' + new Date());
			}
		);
}