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
                    // Default setting for switchAutoCommit
					if($scope.$storage.switchAutoSave === undefined){
						$scope.resetSwitchAutoSave();
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
		$scope.refreshNavBar();
		$route.reload();
	};
	
	$scope.resetSettings = function(){
		// all off
        $.each($scope.$storage.notificationPrefs, function(index, value){ $scope.$storage.notificationPrefs[index] = false });
		$scope.$storage.switchAutoCommit = false;
        $scope.$storage.switchAutoSave = false;
		
		$timeout(function() {
			// reset to default		
			$scope.resetNotificationSettings();
			$scope.resetSwitchAutoCommit();
            $scope.resetSwitchAutoSave();
		}, 500);
	};
	
	$scope.resetNotificationSettings = function(){
		$scope.$storage.notificationPrefs = $.extend($scope.$storage.notificationPrefs, $scope.defaultSettings.notifications);
	};
	
	$scope.resetSwitchAutoCommit = function(){
		$scope.$storage.switchAutoCommit = $scope.defaultSettings.switchAutoCommit;
	};
    
    $scope.resetSwitchAutoSave = function(){
		$scope.$storage.switchAutoSave = $scope.defaultSettings.switchAutoSave;
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
}).directive('myNavbarResize', function ($window, $rootScope, $timeout) {
    return function (scope, element) {
        var w = angular.element($window);
        
        var resizeNavbar = function() {
            $timeout(function(){
                // moving ifc items from dropdown-menu to navbar itself
                while($('#navbar-interfaces').width() < ($('#navbar-wrapper').width() - $('#navbar-options').width())
                        && $('#navbar-interfaces-dropdown-menu').children().length > 0){
                    $("#navbar-interfaces-dropdown-menu").children().first().appendTo("#navbar-interfaces");
                }
                
                // moving ifc items from navbar to dropdown-menu
                while($('#navbar-interfaces').width() > ($('#navbar-wrapper').width() - $('#navbar-options').width())){
                    $("#navbar-interfaces").children().last().prependTo("#navbar-interfaces-dropdown-menu");
                    
                    // show/hide dropdown menu for more interfaces (must be inside loop, because it affects the width of the navbar
                    $('#navbar-interfaces-dropdown').toggleClass('hidden', !$('#navbar-interfaces-dropdown-menu').children().length > 0);
                }
                
                // show/hide dropdown menu when possible
                $('#navbar-interfaces-dropdown').toggleClass('hidden', !$('#navbar-interfaces-dropdown-menu').children().length > 0);
            });
        };
        
        // watch navbar
        $rootScope.$watch('navbar', function() {
            resizeNavbar();
        });
        
        // when window size gets changed
        w.bind('resize', function () {        
            resizeNavbar();
        });
        
        // when page loads
        resizeNavbar();
    }
});