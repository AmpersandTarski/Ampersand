AmpersandApp.controller('ProjectsController', ['$scope', '$rootScope', '$routeParams', 'Restangular', '$timeout', '$modal', function ($scope, $rootScope, $routeParams, Restangular, $timeout, $modal) {
	
	// model (can be changed by view)
	$scope.SESSION = Restangular.one('interface/Projects/atom').get().$object;
	
	// patch function
	$scope.patch = function(){
		$scope.SESSION
			.patch()
			.then(function(data) {
				$rootScope.notifications = data.notifications;
				$scope.SESSION = Restangular.restangularizeElement('', data.content, 'interface/Projects/atom');
				
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
			templateUrl: 'app/views/Projects_addProjectleider.html',
			controller: 'ProjectsController_addProjectleider',
			size: 'lg', // optional 'sm' (small), 'lg' (large)
			backdrop: true, // true, false or 'static'
			resolve: {
				items : function () {
					return $scope.items;
				}
			}
			
		});
		
		modalInstance.result.then(function (selectedId) {
			if(obj[property] == null){
				obj[property] = {};
				obj[property][selectedId] = {'id' : selectedId};
			}else{
				obj[property][selectedId] = {'id' : selectedId};
			}
			console.log('selected: ' + selectedId);
			$scope.patch();
		}, function () {
			console.log('Modal dismissed at: ' + new Date());
		});
		
	}
	
	// function for Datapicker
	$scope.datepicker = [];
	$scope.openDatepicker = function($event, datepicker) {
		$event.preventDefault();
		$event.stopPropagation();
		
		$scope.datepicker[datepicker] = {'open' : true};
	};

	
}]).controller('ProjectsController_addProjectleider', ['$scope', 'Restangular', '$modalInstance', function($scope, Restangular, $modalInstance) {
	
	$scope.Projectleiders = Restangular.all('interface/Person/atoms').getList().$object;
	
	$scope.select = function(id) {
		$scope.selectedId = id;
		console.log('click: ' + id);
	}
	
	$scope.ok = function () {
		$modalInstance.close($scope.selectedId);
	};
	
	$scope.cancel = function () {
		$modalInstance.dismiss('cancel');
	};
	
}]);

