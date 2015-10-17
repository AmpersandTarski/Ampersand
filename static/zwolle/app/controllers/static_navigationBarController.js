AmpersandApp.controller('static_navigationBarController', function ($scope, $rootScope, $route, $routeParams, Restangular, $localStorage, $sessionStorage) {
	
	$scope.$storage = $localStorage;
	$scope.$sessionStorage = $sessionStorage;
	$scope.defaultNotificationSettings = {};
	
	$rootScope.loadingNavBar = new Array(); // initialize an array for promises, used by angular-busy module (loading indicator)
	
	$rootScope.selectRole = function(roleId){
		$localStorage.roleId = roleId;
		
		// refresh navbar + notifications
		$rootScope.refreshNavBar();
		$rootScope.getNotifications();
		//$scope.reload();
	};
	
	$rootScope.selectRoleByLabel = function (roleLabel){
		angular.forEach($scope.navbar.roles, function(role) {
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
			Restangular.one('navbar')
				.get()
				.then(function(data){
					$rootScope.navbar = data;
					$scope.$sessionStorage.session = data.session;
					$scope.$sessionStorage.sessionVars = data.sessionVars;
					
					$scope.defaultNotificationSettings = data.defaultSettings.notifications;
					
					// Default preferences for notifications
					if($scope.$storage.notificationPrefs === undefined){
						$scope.resetNotificationSettings();
					}
				}, function(error){
					// on error
				})
		);
	};
	
	$scope.destroySession = function(){
		session = Restangular.one('session', $scope.$sessionStorage.session.id);
		session.remove().then(function(data){
			$rootScope.updateNotifications(data.notifications);
			
			// set roleId back to 0
			$scope.selectRole(0);
			
		});
	};
	
	$scope.reload = function(){
		$route.reload();
	};
	
	$scope.resetNotificationSettings = function(){
		$scope.$storage.notificationPrefs = $.extend($scope.$storage.notificationPrefs, $scope.defaultNotificationSettings);
		$scope.switchDefaultSettings = true;
	};
	
	$scope.$watch('switchDefaultSettings', function(){
		if($scope.switchDefaultSettings == true) $scope.resetNotificationSettings();
	});
	
	$scope.$watchCollection('$storage.notificationPrefs', function() {
		var isDefault = true;
		if($scope.$storage.notificationPrefs.switchShowViolations != $scope.defaultNotificationSettings.switchShowViolations) isDefault = false;
		if($scope.$storage.notificationPrefs.switchShowSuccesses != $scope.defaultNotificationSettings.switchShowSuccesses) isDefault = false;
		if($scope.$storage.notificationPrefs.switchAutoHideSuccesses != $scope.defaultNotificationSettings.switchAutoHideSuccesses) isDefault = false;
		if($scope.$storage.notificationPrefs.switchShowErrors != $scope.defaultNotificationSettings.switchShowErrors) isDefault = false;
		if($scope.$storage.notificationPrefs.switchShowInvariants != $scope.defaultNotificationSettings.switchShowInvariants) isDefault = false;
		if($scope.$storage.notificationPrefs.switchShowInfos != $scope.defaultNotificationSettings.switchShowInfos) isDefault = false;
		
		if(!isDefault) $scope.switchDefaultSettings = false;
		else $scope.switchDefaultSettings = true;
	});
	
	$rootScope.refreshNavBar(); // initialize navbar
});