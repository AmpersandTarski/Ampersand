AmpersandApp.controller('static_notificationCenterController', function ($scope, $rootScope, $routeParams, $timeout, Restangular) {
		
	Restangular.one('notifications/all').get().then(function(data){
		$rootScope.notifications = data;
	});
	
	$rootScope.updateNotifications = function(notifications){
		$rootScope.notifications = notifications;
		$timeout(function() {
	    	console.log('now.');
	    	$rootScope.notifications.successes = [];
	    }, 3000);
	}
	
	$rootScope.getNotifications = function(){
		Restangular.one('notifications/all').get().then(function(data){
			$rootScope.updateNotifications(data);
		});
	}
	
	$scope.closeAlert = function(alerts, index) {
		alerts.splice(index, 1);
	}
	
	$rootScope.switchShowViolations = true;
	$rootScope.switchShowInfos = false;
	
	$rootScope.toggleShowViolations = function(){
		$rootScope.switchShowViolations = !$rootScope.switchShowViolations;
	}
	
	$rootScope.toggleShowInfos = function(){
		$rootScope.switchShowInfos = !$rootScope.switchShowInfos;
	}
	
});
