AmpersandApp.controller('static_navigationBarController', function ($scope, $rootScope, $route, $routeParams, Restangular, $localStorage) {
	
	$scope.$storage = $localStorage;
	
	$rootScope.interfaces = Restangular.one('interfaces/all').get().$object;
		
	$scope.roles = Restangular.all('roles').getList().$object;
	
	$scope.extensions = Restangular.all('extensions/all').getList().$object;
	
	$scope.selectRole = function(roleId){
		$localStorage.roleId = roleId;
		
		// refresh interfaces list + notifications
		$rootScope.interfaces = Restangular.one('interfaces/all').get().$object;
		$rootScope.notifications = Restangular.one('notifications/all').get().$object;
		$route.reload();
	};
	
	$scope.selectRoleByLabel = function (roleLabel){
		
		angular.forEach($scope.roles, function(role) {
			if(role.label == roleLabel){
				$scope.selectRole(role.id);
				return;
			}
			
			console.log('Unknown role: ' + roleLabel);
			return;
		});
		
	}
	
	$scope.destroySession = function(){
		$rootScope.session.remove().then(function(data){
			$rootScope.updateNotifications(data.notifications);
			$rootScope.session = '';
			$rootScope.session = Restangular.one('session').get().$object;
		});
	}
	
	$scope.reload = function(){
		$route.reload();
	}
	
});