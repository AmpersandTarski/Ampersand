AmpersandApp.controller('PeopleController', function ($scope, $rootScope, $routeParams, Restangular, $timeout, $modal) {
	
	url = 'interface/People/atom';
	if(typeof $routeParams.atom != 'undefined'){
		list = Restangular.one(url, $routeParams.atom).get().then(function(data){
			$scope.ResourceList = Restangular.restangularizeCollection('', data, url);
		});
		
	}else{
		$scope.ResourceList = Restangular.all(url).getList().$object;
	}
	
	// patch function
	$scope.patch = function(ResourceId){
		$scope.ResourceList[ResourceId]
			.patch()
			.then(function(data) {
				$rootScope.notifications = data.notifications;
				$scope.ResourceList[ResourceId] = Restangular.restangularizeElement('', data.content, 'interface/People/atom');
				
				$timeout(function() {
			    	console.log('now');
			    	$rootScope.notifications.successes = [];
			    }, 3000);
			});
		
	}
	
	// function to remove item (key) from list (obj)
	$scope.removeObject = function(obj, key, ResourceId){
		delete obj[key];
		$scope.patch(ResourceId);
	}
	
	// Also needed by addModal
	$scope.addObject = function(obj, property, val, ResourceId){
		if(val === undefined || val == ''){
			console.log('object is undefined');
		}else{
			if(obj[property] === null) obj[property] = {};
			obj[property][val] = {'id': val};
			$scope.patch(ResourceId);
			val = ''; // reset input field
		}
	}
	
	$scope.selected = {}; // used for making selections from typeahead
	$scope.typeahead = {};
	$scope.typeahead.Project = Restangular.all('concept/Project/atoms').getList().$object;

});

