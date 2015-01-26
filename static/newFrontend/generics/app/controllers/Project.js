AmpersandApp.controller('ProjectController', function ($scope, $rootScope, $routeParams, Restangular, $timeout, $modal, $location) {
	
	url = 'interface/Project/atom';
	if($routeParams['new']){
		newAtom = Restangular.one(url).post().then(function (data){
			$scope.ResourceList = Restangular.restangularizeCollection('', data, url);
		});
	}else if(typeof $routeParams.atom != 'undefined'){
		list = Restangular.one(url, $routeParams.atom).get().then(function(data){
			$scope.ResourceList = Restangular.restangularizeCollection('', data, url);
		});
		
	}else{
		$scope.ResourceList = Restangular.all(url).getList().$object;
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
	$scope.removeObject = function(obj, key){
		delete obj[key];
		$scope.patch();
	}
	
	$scope.addObject = function(obj, property, val){
		if(val === undefined || val == ''){
			console.log('object is undefined');
		}else{
			if(obj[property] === null) obj[property] = {};
			obj[property][val] = {'id': val};
			$scope.patch();
			$scope.selected.Theme = ''; // reset input field
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
	
	$scope.addProjectleider = function(obj, property){
		
		var modalInstance = $modal.open({
			templateUrl		: 'generics/app/views/Project_addProjectleider.html',
			controller		: 'ProjectController_addProjectleider',
			size			: 'lg', 			// optional 'sm' (small), 'lg' (large)
			backdrop		: true,				// true, false or 'static'
			// resolve		: { } 				// an optional map of dependencies which should be injected into the controller			
		
		});
		
		modalInstance.result // a promise that is resolved when a modal is closed and rejected when a modal is dismissed
			.then( // then() called when promise is resolved or rejected
				function (selectedId) { // function when modal is closed
					if(obj[property] == null){
						obj[property] = {};
						obj[property][selectedId] = {'id' : selectedId};
					}else{
						obj[property][selectedId] = {'id' : selectedId};
					}
				
					console.log('selected: ' + selectedId);
					$scope.patch();
				
				}, function () { // function when modal is dismissed
					console.log('Modal dismissed at: ' + new Date());
				}
			);
	}
	
	$scope.addProjectmember = function(obj, property){
		
		var modalInstance = $modal.open({
			templateUrl		: 'generics/app/views/Project_addProjectmember.html',
			controller		: 'ProjectController_addProjectmember',
			size			: 'lg', 			// optional 'sm' (small), 'lg' (large)
			backdrop		: true,				// true, false or 'static'
			// resolve		: { } 				// an optional map of dependencies which should be injected into the controller			
		
		});
		
		modalInstance.result // a promise that is resolved when a modal is closed and rejected when a modal is dismissed
			.then( // then() called when promise is resolved or rejected
				function (selectedId) { // function when modal is closed
					if(obj[property] == null){
						obj[property] = {};
						obj[property][selectedId] = {'id' : selectedId};
					}else{
						obj[property][selectedId] = {'id' : selectedId};
					}
				
					console.log('selected: ' + selectedId);
					$scope.patch();
				
				}, function () { // function when modal is dismissed
					console.log('Modal dismissed at: ' + new Date());
				}
			);
	}

}).controller('ProjectController_addProjectleider', ['$scope', 'Restangular', '$modalInstance', function($scope, Restangular, $modalInstance) {
	
	$scope.Projectleiders = Restangular.all('interface/Person/atom').getList().$object;
	
	$scope.select = function(id) {
		console.log('click: ' + id);
		$modalInstance.close(id);
	}
	
	$scope.cancel = function () {
		$modalInstance.dismiss('cancel');
	};
	
}]).controller('ProjectController_addProjectmember', ['$scope', 'Restangular', '$modalInstance', function($scope, Restangular, $modalInstance) {
	
	// api/v1/concept/{concept}/atoms provides for all atoms only id, label and concepttype
	$scope.list = Restangular.all('concept/Person/atom').getList().$object;
	
	$scope.select = function(id) {
		console.log('click: ' + id);
		$modalInstance.close(id);
	}
	
	$scope.cancel = function () {
		$modalInstance.dismiss('cancel');
	};
	
}]);


