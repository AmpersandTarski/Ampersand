AmpersandApp.controller('static_logWindowController', function ($scope, $rootScope) {
		
	$rootScope.switchShowLogWindow = false;
	
	$rootScope.toggleShowLogWindow = function(){
		$rootScope.switchShowLogWindow = !$rootScope.switchShowLogWindow;
	}
	
	$scope.showLogTypes = { 'query' : false };
	
	$scope.showLogEntry = function(log){
		if(log.type != 'QUERY' || $scope.showLogTypes.query == true) return true;
		else return false;
	}

});
