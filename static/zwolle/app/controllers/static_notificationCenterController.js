AmpersandApp.controller('static_notificationCenterController', function ($scope, $rootScope, $route, $routeParams, $timeout, Restangular, $localStorage) {
	
	$scope.$storage = $localStorage;
	
	// Default setting for switchAutoCommit
	if($scope.$storage.switchAutoCommit === undefined){
		$scope.$storage.switchAutoCommit = true;
	}
	
	// Set request type based upon switchAutoCommit
	$rootScope.defaultRequestType = $scope.$storage.switchAutoCommit ? 'promise' : 'feedback';
	$scope.$watch('$storage.switchAutoCommit', function() {
		$rootScope.defaultRequestType = $scope.$storage.switchAutoCommit ? 'promise' : 'feedback';
	});
	
	// Function to update notifications after api response
	$rootScope.updateNotifications = function(notifications){
		// Overwrite
		$rootScope.notifications.logs = notifications.logs;
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
		
		angular.forEach(notifications.logs, function(log) {
			if($scope.$storage.logWindowPrefs.showLogTypes[log.type] == undefined) $scope.$storage.logWindowPrefs.showLogTypes[log.type] = true;
		});
	}
	
	$rootScope.addError = function(message){
		alreadyExists = false;
		arr = $rootScope.notifications.errors;
		for (var i = 0; i < arr.length; i++) {
			if (arr[i].message == message) {
				arr[i].count += 1;
				alreadyExists = true;
			}
		}
		if(!alreadyExists) $rootScope.notifications.errors.push( {'message' : message, 'count' : 1} );
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
		Restangular.one('notifications/all').get().then(function(data){
			$rootScope.updateNotifications(data);
		});
	}
	
	// Hide success-, error-, info- and invariant violation messages (not process rule violations) upon route change
	$scope.$on("$routeChangeSuccess", function(){
		$rootScope.notifications.successes = [];
		$rootScope.notifications.errors = [];
		$rootScope.notifications.infos = [];
		$rootScope.notifications.invariants = [];
	});
	
	// Function to close notifications
	$scope.closeAlert = function(alerts, index) {
		alerts.splice(index, 1);
	}
	
});
