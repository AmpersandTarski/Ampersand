AmpersandApp.controller('ProjectsController', function ($scope, $rootScope, $routeParams, Restangular, $location, $modal) {
	
	// URL to the interface API. 'http://pathToApp/api/v1/' is already configured elsewhere.
	url = 'interface/Projects';
	
	// Only insert code below if interface is allowed to create new atoms. This is not specified in interfaces yet, so add by default
	if($routeParams['new']){
		newAtom = Restangular.one(url).post().then(function (data){
			$scope.ResourceList = Restangular.restangularizeCollection('', data, url);
		});
	}else
	
	// Checks if resourceId is provided, and if so does a get() else a getList()
	if(typeof $routeParams.resourceId != 'undefined'){
		list = Restangular.one(url, $routeParams.resourceId).get().then(function(data){
			$scope.ResourceList = Restangular.restangularizeCollection('', data, url);
		});
	}else{
		$scope.ResourceList = Restangular.all(url).getList().$object;
	}
	
	// Patch function to update a Resource
	$scope.patch = function(ResourceId){
		$scope.ResourceList[ResourceId]
			.patch()
			.then(function(data) {
				$rootScope.updateNotifications(data.notifications);
				$scope.ResourceList[ResourceId] = Restangular.restangularizeElement('', data.content, url);
			});
	}
	
	// RemoveObject function to remove an item (key) from list (obj).
	$scope.removeObject = function(obj, key, ResourceId){
		delete obj[key];
		$scope.patch(ResourceId);
	}
	
	// AddObject function to add a new item (val) to a certain property (property) of an object (obj)
	// Also needed by addModal function.
	$scope.addObject = function(obj, property, selected, ResourceId){
		if(selected.id === undefined || selected.id == ''){
			console.log('selected id is undefined');
		}else{
			if(obj[property] === null) obj[property] = {};
			obj[property][selected.id] = {'id': selected.id};
			selected.id = ''; // reset input field
			$scope.patch(ResourceId);
		}
	}
	
	// Function for Datapicker
	$scope.datepicker = []; // empty array to administer if datepickers (can be multiple on one page) are open and closed
	$scope.openDatepicker = function($event, datepicker) {
		$event.preventDefault();
		$event.stopPropagation();
		
		$scope.datepicker[datepicker] = {'open' : true};
	}
	
	// PopUp function
	$scope.popUp = function(obj, property, ResourceId){
		var modalInstance = $modal.open({
			templateUrl		: 'generics/app/views/Projects_addProjectleider.html',
			controller		: 'static_addModalController',
			size			: 'lg', 			// optional 'sm' (small), 'lg' (large)
			backdrop		: true,				// true, false or 'static'
			resolve			: { restUrl: function () { return 'interface/Person'; } }	// an optional map of dependencies which should be injected into the controller			
		});
		
		modalInstance.result // a promise that is resolved when a modal is closed and rejected when a modal is dismissed
			.then( // then() called when promise is resolved or rejected
				function (selectedId) { // function when modal is closed
					console.log('selected: ' + selectedId);
					selected = {id : selectedId};
					$scope.addObject(obj, property, selected, ResourceId);
				}, function () { // function when modal is dismissed
					console.log('Modal dismissed at: ' + new Date());
				}
			);
	}
});

