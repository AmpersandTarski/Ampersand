AmpersandApp.controller('static_breadcrumbController', function ($scope, $route, $location, $rootScope) {
	$scope.history = [];
	
	$scope.$on("$routeChangeSuccess", function(event, current, previous){
		
		indexInHistory = linkInArray($scope.history, $location.path())
		indexInTopLevel = linkInArray(($rootScope.navbar || {}).top, $location.path())
		if(indexInHistory !== false) $scope.history.splice(indexInHistory, $scope.history.length - indexInHistory); 
		
		// Clear history
		if($location.path() == '/' || indexInTopLevel !== false){ // i.e. 'Home'
			$scope.history = []; // empty history
		}

		// Add new breadcrumb item (except for home path '/')
		if($location.path() != '/'){
			$scope.history.push({ 'link' : $location.path() 
								, 'scope': current.scope
								, 'resourceId' : current.params.resourceId
								, 'interfaceLabel' : current.$$route.interfaceLabel
								});
		}
		
		// Prevent endless breadcrumb
		if($scope.history.length > 5){
			$scope.history.splice(0, $scope.history.length - 5);
			$scope.morethanfive = true;
		}else{
			$scope.morethanfive = false;
		}
	});
	
	function linkInArray(arr, link) {
		if(arr === undefined) return false;
		
		for (var i = 0; i < arr.length; i++) {
			if (arr[i].link == link) {
				return i;
			}
		}
		return false;
	}
});