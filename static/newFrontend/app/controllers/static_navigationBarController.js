AmpersandApp.controller('static_navigationBarController', function ($scope, $rootScope, $route, $routeParams, Restangular, $localStorage) {
	
	$scope.$storage = $localStorage;
	
	$rootScope.myPromises = new Array(); // initialize an array for promises, used by angular-busy module (loading indicator)
	
	$rootScope.selectRole = function(roleId){
		$localStorage.roleId = roleId;
		
		// refresh navbar + notifications
		$rootScope.refreshNavBar();
		$rootScope.getNotifications();
		$scope.reload();
	};
	
	$scope.selectRoleByLabel = function (roleLabel){
		angular.forEach($scope.navbar.roles, function(role) {
			if(role.label == roleLabel){
				$scope.selectRole(role.id);
				return;
			}
			
			$rootScope.addError('Unknown role: ' + roleLabel);
			return;
		});
	};
	
	$rootScope.refreshNavBar = function(){
		$rootScope.myPromises.push(
			Restangular.one('navbar')
				.get()
				.then(function(data){
					$rootScope.navbar = data;
				}, function(error){
					// on error
				})
		);
	};
	
	$scope.destroySession = function(){
		$rootScope.session.remove().then(function(data){
			$rootScope.updateNotifications(data.notifications);
			$rootScope.session = '';
			
			// set roleId back to 0
			$scope.selectRole(0);
			
			$rootScope.session = Restangular.one('session').get().$object;
		});
	};
	
	$scope.reload = function(){
		$route.reload();
	};
	
	$rootScope.refreshNavBar(); // initialize navbar
});