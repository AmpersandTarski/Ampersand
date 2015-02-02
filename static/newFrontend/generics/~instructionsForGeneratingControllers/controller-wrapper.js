/*
 * This is the wrapper for every controller
 * 
 */

AmpersandApp.controller('<interfaceName>Controller', function ($scope, $rootScope, $routeParams, Restangular, $location) {
	
	// URL to the interface API. 'http://pathToApp/api/v1/' is already configured elsewhere.
	url = 'interface/<interfaceName>';
	
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
	
	
	// Here other content for the controller
	
	
});