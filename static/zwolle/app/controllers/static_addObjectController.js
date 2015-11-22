AmpersandApp.controller('addObjectController', function($scope, $rootScope, Restangular){
	
	$scope.selected = {}; // an empty object for temporary storing the typeahead selection
	
	$scope.hasNoResults = false;
	
	// Regular function used by Atomic-OBJECT template
	$scope.typeaheadOnSelect = function ($item, $model, $label, obj, property, resourceId){
		$scope.addObject(obj, property, $item, resourceId);
		$scope.hasNoResults = false;
	};
	
	// Add object as new resource in interface
	$scope.addExistingResourceOnSelect = function ($item, $model, $label, obj, property, prepend, requestType){
		if(prepend === 'undefined') var prepend = false;
		requestType = requestType || $rootScope.defaultRequestType; // set requestType. This does not work if you want to pass in a falsey value i.e. false, null, undefined, 0 or ""
		
		// Post to backend		
		$scope.loadingInterface.push(
			$scope.srcAtom.all(property)
				.post($item, {'requestType' : requestType})
				.then(function(data){ // POST
					$rootScope.updateNotifications(data.notifications);
					
					if(prepend) obj[property].unshift(Restangular.restangularizeElement($scope.srcAtom, data.content, property)); // Add to collection
					else obj[property].push(Restangular.restangularizeElement($scope.srcAtom, data.content, property)); // Add to collection
					
					showHideButtons(data.invariantRulesHold, data.requestType, data.content.id);
				})
		);
		
		$scope.hasNoResults = false;
		
	};
	
});