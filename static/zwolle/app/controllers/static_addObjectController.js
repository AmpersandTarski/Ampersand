AmpersandApp.controller('addObjectController', function($scope){
	
	$scope.selected = {}; // an empty object for temporary storing the typeahead selection
	
	$scope.hasNoResults = false;
	
	// Regular function used by Atomic-OBJECT template
	$scope.typeaheadOnSelect = function ($item, $model, $label, obj, property, resourceId){
		$scope.addObject(obj, property, $item, resourceId);
		$scope.hasNoResults = false;
	};
});