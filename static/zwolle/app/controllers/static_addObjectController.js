AmpersandApp.controller('addObjectController', function($scope, $rootScope, Restangular){
	
	$scope.selected = {}; // an empty object for temporary storing the typeahead selection
	
	$scope.hasNoResults = false;
	
	// Regular function used by Atomic-OBJECT template
	$scope.typeaheadOnSelect = function ($item, $model, $label, obj, property, patchResource){
		$scope.addObject(obj, property, $item, patchResource);
		$scope.hasNoResults = false;
	};
	
});