AmpersandApp.controller('ProjectsController', ['$scope', '$routeParams', 'ProjectsFactory', function ($scope, $routeParams, ProjectsFactory) {
	$scope.atom = $routeParams.atom;
	$scope.SESSION = ProjectsFactory.get({atom : $scope.atom}, function(){
		// function when GET request is succeeded.
	});
	
	$scope.save = function(){
		$scope.Projects.$save({atom : $scope.atom});	// .... save data to server
	}
	
}]);

AmpersandApp.factory('ProjectsFactory', ['$resource', function($resource){
	var url = 'api/v1/interface/Projects/atom/:atom.json';
	
	return $resource(url, {}, {
		get: {method:'GET'},
		save: {method:'POST'}
	});
}]);