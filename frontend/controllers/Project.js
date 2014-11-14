AmpersandApp.controller('ProjectController', ['$scope', '$routeParams', 'ProjectFactory', function ($scope, $routeParams, ProjectFactory) {
	$scope.atom = $routeParams.atom;
	$scope.Project = ProjectFactory.get({atom : $scope.atom}, function(){
		// function when GET request is succeeded.
	});
	
	$scope.save = function(){
		$scope.Project.$save({atom : $scope.atom});	// .... save data to server
	}
	
}]);

AmpersandApp.factory('ProjectFactory', ['$resource', function($resource){
	var url = 'api/v1/interface/Project/atom/:atom.json';
	
	return $resource(url, {}, {
		get: {method:'GET'},
		save: {method:'POST'}
	});
}]);