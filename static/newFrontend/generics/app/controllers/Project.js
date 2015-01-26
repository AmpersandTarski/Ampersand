AmpersandApp.controller('ProjectController', function ($scope, $rootScope, $routeParams, Restangular, $timeout, $modal, $location) {
	
	$scope.url = 'interface/Project/atom';
	if($routeParams['new']){
		newAtom = Restangular.one($scope.url).post().then(function (data){
			$scope.ResourceList = Restangular.restangularizeCollection('', data, $scope.url);
		});
	}else if(typeof $routeParams.atom != 'undefined'){
		list = Restangular.one($scope.url, $routeParams.atom).get().then(function(data){
			$scope.ResourceList = Restangular.restangularizeCollection('', data, $scope.url);
		});
		
	}else{
		$scope.ResourceList = Restangular.all($scope.url).getList().$object;
	}
	
	$scope.patch = function(ResourceId){
		$scope.ResourceList[ResourceId]
			.patch()
			.then(function(data) {
				$rootScope.notifications = data.notifications;
				$scope.ResourceList[ResourceId] = Restangular.restangularizeElement('', data.content, 'interface/Project/atom');
				
				$timeout(function() {
			    	console.log('now');
			    	$rootScope.notifications.successes = [];
			    }, 3000);
			});
	}
	
	$scope.deleteAtom = function (ResourceId){
		if(confirm('Are you sure?')){
			$scope.ResourceList[ResourceId]
				.remove()
				.then(function(data){
					$rootScope.notifications = data.notifications;
					$location.url('/');
				});
		}
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
	
	// function for Datapicker
	$scope.datepicker = [];
	$scope.openDatepicker = function($event, datepicker) {
		$event.preventDefault();
		$event.stopPropagation();
		
		$scope.datepicker[datepicker] = {'open' : true};
	};
	
	$scope.selected = {}; // used for making selections from typeahead
	$scope.typeahead = {};
	$scope.typeahead.Theme = Restangular.all('concept/Theme/atoms').getList().$object;
	$scope.typeahead.Person = Restangular.all('concept/Person/atoms').getList().$object;
	
	$scope.addModal = function(obj, property, ResourceId){
		var modalInstance = $modal.open({
			templateUrl		: 'generics/app/views/Project_addProjectleider.html',
			controller		: 'static_addModalController',
			size			: 'lg', 			// optional 'sm' (small), 'lg' (large)
			backdrop		: true,				// true, false or 'static'
			resolve			: { restUrl: function () { return 'interface/Person/atom'; } }	// an optional map of dependencies which should be injected into the controller			
		});
		
		modalInstance.result // a promise that is resolved when a modal is closed and rejected when a modal is dismissed
			.then( // then() called when promise is resolved or rejected
				function (selectedId) { // function when modal is closed
					console.log('selected: ' + selectedId);
					$scope.addObject(obj, property, selectedId, ResourceId);
				}, function () { // function when modal is dismissed
					console.log('Modal dismissed at: ' + new Date());
				}
			);
	}
	
});


