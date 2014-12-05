AmpersandApp.controller('ProjectController', ['$scope', '$rootScope', '$routeParams', 'ObjectFactory', function ($scope, $rootScope, $routeParams, ObjectFactory) {
	$scope.atom1 = $routeParams.atom1;
	$scope.atom2 = $routeParams.atom2;
	
	$scope.Project = ObjectFactory.get({atom : $scope.atom1, interface : 'Project'}, function(){
		// function when GET request is succeeded.
	});
	$scope.Person = ObjectFactory.get({atom : $scope.atom2, interface : 'Person'}, function(){
		// function when GET request is succeeded.
	});
	
	$scope.save = function(){
		$scope.Project.$save({atom : $scope.atom1}, function(data){
			//success callback function
			$rootScope.notifications = data.notifications;
			
		});
	}

}]);

AmpersandApp.factory('ObjectFactory', ['$resource', function($resource){
	var url = 'api/v1/interface/:interface/atom/:atom.json';
	
	return $resource(url, {}, {
		get: {method:'GET'},
		save: {method:'PUT'}
	});
}]);
