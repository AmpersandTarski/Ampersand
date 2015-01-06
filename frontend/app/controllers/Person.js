AmpersandApp.controller('PersonController', ['$scope', '$rootScope', '$routeParams', 'Restangular', '$timeout', '$modal', function ($scope, $rootScope, $routeParams, Restangular, $timeout, $modal) {
	
	// model (can be changed by view)
	$scope.Person = Restangular.one('interface/Person/atom', $routeParams.atom).get().$object;
	
	$scope.patch = function(){
		$scope.Person
			.patch()
			.then(function(data) {
				$rootScope.notifications = data.notifications;
				$scope.Person = Restangular.restangularizeElement('', data.content, 'interface/Person/atom');
				
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