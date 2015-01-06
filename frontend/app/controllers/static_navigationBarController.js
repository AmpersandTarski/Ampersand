AmpersandApp.controller('static_navigationBarController', ['$scope', '$rootScope', '$routeParams', 'Restangular', function ($scope, $rootScope, $routeParams, Restangular) {
	$scope.session_id = session_id;
	
	$scope.interfaces = Restangular.all('interfaces/top').getList().$object;
		
	$scope.roles = Restangular.all('roles/all').getList().$object;
	
	$scope.extensions = Restangular.all('extensions/all').getList().$object;
	
}]);