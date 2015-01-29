AmpersandApp.controller('PersonController', function ($scope, $rootScope, $routeParams, Restangular, $location, $modal) {
	
	// URL to the interface API. 'http://pathToApp/api/v1/' is already configured elsewhere.
	url = 'interface/Person/atom';
	
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
	
	// Delete function to delete a complete Resource
	$scope.deleteResource = function (ResourceId){
		if(confirm('Are you sure?')){
			$scope.ResourceList[ResourceId]
				.remove()
				.then(function(data){
					$rootScope.updateNotifications(data.notifications);
					$location.url('/');
				});
		}
	}
	
	// RemoveObject function to remove an item (key) from list (obj).
	$scope.removeObject = function(obj, key, ResourceId){
		delete obj[key];
		$scope.patch(ResourceId);
	}

});