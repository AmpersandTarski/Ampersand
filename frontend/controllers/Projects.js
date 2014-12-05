AmpersandApp.controller('ProjectsController', ['$scope', '$routeParams', 'ProjectsFactory', function ($scope, $routeParams, ProjectsFactory) {
	$scope.atom1 = $routeParams.atom1;
	$scope.SESSION = ProjectsFactory.get({atom : $scope.atom1}, function(){
		// function when GET request is succeeded.
	});
	
	$scope.save = function(){
		$scope.SESSION.$save({atom : $scope.atom1});	// .... save data to server
	}
	
}]);

AmpersandApp.factory('ProjectsFactory', ['$resource', function($resource){
	var url = 'api/v1/interface/Projects/atom/:atom.json';
	
	return $resource(url, {}, {
		get: {method:'GET'},
		save: {method:'POST'}
	});
}]);