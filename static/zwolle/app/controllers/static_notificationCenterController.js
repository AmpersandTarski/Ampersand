AmpersandApp.controller('static_notificationCenterController', function ($scope, $rootScope, $route, $routeParams, $timeout, Restangular, $localStorage, $sessionStorage) {
	
	$scope.$storage = $localStorage;
	$scope.$sessionStorage = $sessionStorage;
	
	// Initialize notifications container
	$rootScope.notifications =  { 'violations' 	: []
								, 'invariants' 	: []
								, 'infos' 		: []
								, 'successes' 	: []
								, 'errors' 		: []
								};
	
	// Function to update notifications after api response
	$rootScope.updateNotifications = function(notifications){
		if(notifications === undefined) notifications = {};
		
		// Overwrite
		$rootScope.notifications.violations = notifications.violations;
		$rootScope.notifications.invariants = notifications.invariants;
		$rootScope.notifications.infos = notifications.infos;
		
		// Merge
		$rootScope.notifications.successes = $rootScope.notifications.successes.concat(notifications.successes);
		$rootScope.notifications.errors = $rootScope.notifications.errors.concat(notifications.errors);
		
		if($scope.$storage.notificationPrefs.switchAutoHideSuccesses){
			$timeout(function() {
		    	$rootScope.notifications.successes = [];
		    }, 3000);
		}
	}
	
	$rootScope.addError = function(message, code, persistent, details){
		code = typeof code !== undefined ? code : null;
		persistent = typeof persistent !== undefined ? persistent : false;
		details = typeof details !== undefined ? details : false;
		
		alreadyExists = false;
		arr = $rootScope.notifications.errors;
		for (var i = 0; i < arr.length; i++) {
			if (arr[i].message == message) {
				arr[i].count += 1;
				arr[i].code = code;
				arr[i].persistent = persistent;
				arr[i].details = details
				alreadyExists = true;
			}
		}
		if(!alreadyExists) $rootScope.notifications.errors.push( {'message' : message, 'code' : code, 'count' : 1, 'persistent' : persistent, 'details' : details} );
	}
	
	$rootScope.addInfo = function(message){
		alreadyExists = false;
		arr = $rootScope.notifications.infos;
		for (var i = 0; i < arr.length; i++) {
			if (arr[i].message == message) {
				arr[i].count += 1;
				alreadyExists = true;
			}
		}
		if(!alreadyExists) $rootScope.notifications.infos.push( {'message' : message, 'count' : 1} );
	}
	
	// Function to get notifications again
	$rootScope.getNotifications = function(){
		Restangular.one('sessions', $scope.$sessionStorage.session.id).one('notifications').get().then(function(data){
			$rootScope.updateNotifications(data);
		});
	}
	
	// Hide success-, error-, info- and invariant violation messages (not process rule violations) upon route change
	$scope.$on("$routeChangeSuccess", function(){
		$rootScope.notifications.successes = [];
		$rootScope.notifications.errors = $rootScope.notifications.errors.filter(function (error){
			if(error.persistent){
				error.persistent = false;
				return true;
			}
			else return false;
		});
		$rootScope.notifications.infos = [];
		$rootScope.notifications.invariants = [];
	});
	
	// Function to close notifications
	$scope.closeAlert = function(alerts, index) {
		alerts.splice(index, 1);
	}
	
});
