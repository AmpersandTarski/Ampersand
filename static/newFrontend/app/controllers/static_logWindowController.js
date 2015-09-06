AmpersandApp.controller('static_logWindowController', function ($scope, $rootScope, $localStorage) {
	$scope.$storage = $localStorage;
	
	// Default preferences for log window
	if($scope.$storage.logWindowPrefs === undefined){
		$scope.$storage.logWindowPrefs = {
			  switchShowLogWindow 		: false
			, showLogTypes				: { 'QUERY' : false}
		}
	}

	$scope.showLogEntry = function(log){
		if($scope.$storage.logWindowPrefs.showLogTypes[log.type] || $scope.$storage.logWindowPrefs.showLogTypes[log.type] == undefined) return true;
		else return false;
	}

});