AmpersandApp.controller('static_navigationBarController', function ($scope, $rootScope, $route, $routeParams, Restangular, $localStorage, $sessionStorage, $timeout) {
	
	$scope.$storage = $localStorage;
	$scope.$sessionStorage = $sessionStorage;
	$scope.defaultSettings = {};
	
	$rootScope.loadingNavBar = new Array(); // initialize an array for promises, used by angular-busy module (loading indicator)
	
	$rootScope.selectRole = function(roleId){
		$rootScope.toggleRole(roleId, true);
	};
	
	$rootScope.toggleRole = function(roleId, set){
		angular.forEach($scope.$sessionStorage.sessionRoles, function(role) {
			if (role.id == roleId) {
				if(set === undefined){
					role.active = !role.active;
				}else{
					role.active = set;
				}
			}
		});
		
		// refresh navbar + notifications
		$rootScope.refreshNavBar();
	}
	
	$rootScope.deactivateAllRoles = function(){
		angular.forEach($scope.$sessionStorage.sessionRoles, function(role) {
			role.active = false;
		});
		$rootScope.refreshNavBar();
	}
	
	$rootScope.getActiveRoleIds = function(){
		var roleIds = [];
		angular.forEach($scope.$sessionStorage.sessionRoles, function(role) {
			if (role.active == true) {
				roleIds.push(role.id);
			}
		});
		return roleIds;
	}
	
	$rootScope.selectRoleByLabel = function (roleLabel){
		angular.forEach($scope.sessionStorage.sessionRoles, function(role) {
			if(role.label == roleLabel){
				$rootScope.selectRole(role.id);
				return;
			}
			
			$rootScope.addError('Unknown role: ' + roleLabel);
			return;
		});
	};
	
	$rootScope.refreshNavBar = function(){
		$rootScope.loadingNavBar = new Array();
		$rootScope.loadingNavBar.push(
			Restangular.one('sessions', $scope.$sessionStorage.session.id).one('navbar')
				.get()
				.then(function(data){
					$rootScope.navbar = data;
					$scope.$sessionStorage.session = data.session;
					$scope.$sessionStorage.sessionRoles = data.sessionRoles;
					$scope.$sessionStorage.sessionVars = data.sessionVars;
					
					$scope.defaultSettings = data.defaultSettings;
					
					// Default settings for notificationPrefs
					if($scope.$storage.notificationPrefs === undefined){
						$scope.resetNotificationSettings();
					}
					// Default setting for switchAutoCommit
					if($scope.$storage.switchAutoCommit === undefined){
						$scope.resetSwitchAutoCommit();
					}
					
					// Default setting for cacheGetCalls
					if($scope.$storage.cacheGetCalls === undefined){
						$scope.$storage.cacheGetCalls = $scope.defaultSettings.cacheGetCalls;
					}
					
					// Update notifications
					$rootScope.updateNotifications(data.notifications);
				}, function(error){
					// on error
				})
		);
	};
	
	$scope.destroySession = function(){
		session = Restangular.one('sessions', $scope.$sessionStorage.session.id);
		session.remove().then(function(data){
			$rootScope.updateNotifications(data.notifications);
			
			// deactivate roles
			$rootScope.deactivateAllRoles();
			
		});
	};
	
	$scope.reload = function(){
		$route.reload();
	};
	
	$scope.resetSettings = function(){
		// all off
		$scope.$storage.switchAutoCommit = false;
		$.each($scope.$storage.notificationPrefs, function(index, value){ $scope.$storage.notificationPrefs[index] = false });
		
		$timeout(function() {
			// reset to default		
			$scope.resetNotificationSettings();
			$scope.resetSwitchAutoCommit();
		}, 500);
	};
	
	$scope.resetNotificationSettings = function(){
		$scope.$storage.notificationPrefs = $.extend($scope.$storage.notificationPrefs, $scope.defaultSettings.notifications);
	};
	
	$scope.resetSwitchAutoCommit = function(){
		$scope.$storage.switchAutoCommit = $scope.defaultSettings.switchAutoCommit;
	};
	
	// Set request type based upon switchAutoCommit
	$rootScope.defaultRequestType = $scope.$storage.switchAutoCommit ? 'promise' : 'feedback';
	$scope.$watch('$storage.switchAutoCommit', function() {
		$rootScope.defaultRequestType = $scope.$storage.switchAutoCommit ? 'promise' : 'feedback';
	});
	
	$scope.$watch('$storage.cacheGetCalls', function() {
		Restangular.setDefaultHttpFields({cache: $scope.$storage.cacheGetCalls });
	});
	
	$rootScope.refreshNavBar(); // initialize navbar
});