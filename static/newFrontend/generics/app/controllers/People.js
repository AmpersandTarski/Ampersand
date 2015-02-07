AmpersandApp.controller('PeopleController', function ($scope, $rootScope, $routeParams, Restangular, $location, $modal) {
	
	// URL to the interface API. 'http://pathToApp/api/v1/' is already configured elsewhere.
	url = 'interface/People';
	
	// Only insert code below if interface is allowed to create new atoms. This is not specified in interfaces yet, so add by default
	if($routeParams['new']){
		newAtom = Restangular.one(url).post().then(function (data){
			$scope.ResourceList = Restangular.restangularizeCollection('', data, url);
		});
	}else
	
	// Checks if resourceId is provided, and if so does a get() else a getList()
	if(typeof $routeParams.resourceId != 'undefined'){
		list = Restangular.one(url, $routeParams.resourceId).get().then(function(data){
			$scope.ResourceList = Restangular.restangularizeCollection('', data, url);
		});
	}else{
		$scope.ResourceList = Restangular.all(url).getList().$object;
	}
	
	// Patch function to update a Resource
	$scope.patch = function(ResourceId){
		$scope.ResourceList[ResourceId]
			.patch()
			.then(function(data) {
				$rootScope.updateNotifications(data.notifications);
				$scope.ResourceList[ResourceId] = Restangular.restangularizeElement('', data.content, url);
			});
	}
	
	// RemoveObject function to remove an item (key) from list (obj).
	$scope.removeObject = function(obj, key, ResourceId){
		delete obj[key];
		$scope.patch(ResourceId);
	}
	
	// AddObject function to add a new item (val) to a certain property (property) of an object (obj)
	// Also needed by addModal function.
	$scope.addObject = function(obj, property, selected, ResourceId){
		if(selected.id === undefined || selected.id == ''){
			console.log('selected id is undefined');
		}else{
			if(obj[property] === null) obj[property] = {};
			obj[property][selected.id] = {'id': selected.id};
			selected.id = ''; // reset input field
			$scope.patch(ResourceId);
		}
	}
	
	// Typeahead functionality
	$scope.selected = {}; // an empty object for temporary storing typeahead selections
	$scope.typeahead = {}; // an empty object for typeahead
	$scope.typeahead.Project = Restangular.all('resource/Project').getList().$object;

});

