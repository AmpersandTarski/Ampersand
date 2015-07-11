AmpersandApp.controller('static_breadcrumbController', function ($scope, $route, $location) {
	$scope.history = [];
	
	$scope.$on("$routeChangeSuccess", function(event, current, previous){
		
		i = inHistory($scope.history, $location.path())
		if(i !== false) $scope.history.splice(i, $scope.history.length - i); 
		
		if($location.path() == '/'){ // i.e. 'Home'
			$scope.history = []; // empty history
		}else{
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
	
	function inHistory(arr, link) {
		for (var i = 0; i < arr.length; i++) {
			if (arr[i].link == link) {
				return i;
			}
		}
		return false;
	}
});