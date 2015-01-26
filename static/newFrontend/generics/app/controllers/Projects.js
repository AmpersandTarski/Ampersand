AmpersandApp.controller('ProjectsController', ['$scope', '$rootScope', '$routeParams', 'Restangular', '$timeout', '$modal', function ($scope, $rootScope, $routeParams, Restangular, $timeout, $modal) {
	
	url = 'interface/Projects/atom';
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
				$scope.ResourceList[ResourceId] = Restangular.restangularizeElement('', data.content, 'interface/Projects/atom');
				
				$timeout(function() {
			    	console.log('now');
			    	$rootScope.notifications.successes = [];
			    }, 3000);
			});
		
	}
	
	// function to remove item (key) from list (obj)
	$scope.removeObject = function(obj, key){
		delete obj[key];
		$scope.patch();
	}
	
	$scope.addProjectleider = function(obj, property){
		
		var modalInstance = $modal.open({
			templateUrl		: 'generics/app/views/Projects_addProjectleider.html',
			controller		: 'ProjectsController_addProjectleider',
			size			: 'lg', 			// optional 'sm' (small), 'lg' (large)
			backdrop		: true,				// true, false or 'static'
			// resolve		: { } 				// an optional map of dependencies which should be injected into the controller			
		
		});
		
		modalInstance.result // a promise that is resolved when a modal is closed and rejected when a modal is dismissed
			.then( // then() called when promise is resolved or rejected
				function (selectedId) { // function when modal is closed
					if(obj[property] === null) obj[property] = {};
					obj[property][selectedId] = {'id' : selectedId};
				
					console.log('selected: ' + selectedId);
					$scope.patch();
				
				}, function () { // function when modal is dismissed
					console.log('Modal dismissed at: ' + new Date());
				}
			);
	}
	
	// function for Datapicker
	$scope.datepicker = [];
	$scope.openDatepicker = function($event, datepicker) {
		$event.preventDefault();
		$event.stopPropagation();
		
		$scope.datepicker[datepicker] = {'open' : true};
	};

	
}]).controller('ProjectsController_addProjectleider', ['$scope', 'Restangular', '$modalInstance', function($scope, Restangular, $modalInstance) {
	
	$scope.Projectleiders = Restangular.all('interface/Person/atom').getList().$object;
	
	$scope.select = function(id) {
		console.log('click: ' + id);
		$modalInstance.close(id);
	}
	
	$scope.cancel = function () {
		$modalInstance.dismiss('cancel');
	};
	
}]);

