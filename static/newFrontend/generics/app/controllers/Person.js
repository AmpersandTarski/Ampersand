AmpersandApp.controller('PersonController', function ($scope, $rootScope, $routeParams, Restangular, $timeout, $modal, $location) {
	
	url = 'interface/Person/atom';
	if($routeParams['new']){
		newAtom = Restangular.one(url).post().then(function (data){
			$scope.ResourceList = Restangular.restangularizeCollection('', data, url);
		});
	}else if(typeof $routeParams.atom != 'undefined'){
		list = Restangular.one(url, $routeParams.atom).get().then(function(data){
			$scope.ResourceList = Restangular.restangularizeCollection('', data, url);
		});
	}else{
		$scope.ResourceList = Restangular.all(url).getList().$object;
	}
	
	$scope.patch = function(ResourceId){
		$scope.ResourceList[ResourceId]
			.patch()
			.then(function(data) {
				$rootScope.notifications = data.notifications;
				$scope.Resource = Restangular.restangularizeElement('', data.content, 'interface/Person/atom');
				
				$timeout(function() {
			    	console.log('now');
			    	$rootScope.notifications.successes = [];
			    }, 3000);
			});
		
	}
	
	$scope.deleteAtom = function (ResourceId){
		if(confirm('Are you sure?')){
			$scope.ResourceList[ResourceId]
				.remove()
				.then(function(data){
					$rootScope.notifications = data.notifications;
					$location.url('/');
				});
		}
	}
	
	// function to remove item (key) from list (obj)
	$scope.removeObject = function(obj, key){
		delete obj[key];
		$scope.patch();
	}

});