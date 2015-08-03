AmpersandApp.controller('static_logWindowController', function ($scope, $rootScope) {
		
	$rootScope.switchShowLogWindow = false;
	
	$rootScope.toggleShowLogWindow = function(){
		$rootScope.switchShowLogWindow = !$rootScope.switchShowLogWindow;
	}
	
	// Default for showing log types
	$rootScope.showLogTypes = { 'QUERY' : false};

	$scope.showLogEntry = function(log){
		if($rootScope.showLogTypes[log.type] || $rootScope.showLogTypes[log.type] == undefined) return true;
		else return false;
	}

});
