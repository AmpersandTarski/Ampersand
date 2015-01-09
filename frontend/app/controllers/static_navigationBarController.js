AmpersandApp.controller('static_navigationBarController', ['$scope', '$rootScope', '$routeParams', 'Restangular', function ($scope, $rootScope, $routeParams, Restangular) {
	
	$scope.session = Restangular.one('session').get().$object;
	
	$scope.interfaces = Restangular.all('interfaces/top').getList().$object;
		
	$scope.roles = Restangular.all('roles/all').getList().$object;
	
	$scope.extensions = Restangular.all('extensions/all').getList().$object;
	
	$scope.selectRole = function(roleId){
		$rootScope.roleId = roleId;
		
		// refresh interfaces list + notifications
		$scope.interfaces = Restangular.all('interfaces/top').getList().$object;
		$rootScope.notifications = Restangular.one('notifications/all').get().$object;
	};
	
	$scope.destroySession = function(){
		$scope.session.remove();
	}
	
}]);