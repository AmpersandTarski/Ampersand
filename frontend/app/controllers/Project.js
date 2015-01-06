AmpersandApp.controller('ProjectController', ['$scope', '$rootScope', '$routeParams', 'Restangular', '$timeout', '$modal', function ($scope, $rootScope, $routeParams, Restangular, $timeout, $modal) {
	
	// model (can be changed by view)
	$scope.Project = Restangular.one('interface/Project/atom', $routeParams.atom).get().$object;
	
	$scope.patch = function(){
		$scope.Project
			.patch()
			.then(function(data) {
				$rootScope.notifications = data.notifications;
				$scope.Project = Restangular.restangularizeElement('', data.content, 'interface/Project/atom');
				
				$timeout(function() {
			    	console.log('now');
			    	$rootScope.notifications.successes = [];
			    }, 3000);
			});
		
	}
	
	// function to remove item (key) from list (obj)
	$scope.removeObject = function(obj, key){
		delete obj[key];
		$scope.patch();
	}

}]);
