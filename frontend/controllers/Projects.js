AmpersandApp.controller('ProjectsController', ['$scope', '$rootScope', '$routeParams', 'Restangular', 'createDialog', '$timeout', function ($scope, $rootScope, $routeParams, Restangular, createDialogService, $timeout) {
	
	// model (can be changed by view)
	$scope.SESSION = Restangular.one('interface/Projects/atom', $routeParams.atom1).get().$object;
	
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
	
	$scope.removeObject = function(obj, key){
		delete obj[key];
		$scope.patch();
	}
	
	$scope.addObject = function(){
		
		createDialogService('views/interfaces/Projects_addProjectleider.html',{
		    id : '',
		    title: 'Find projectleider',
		    backdrop: true,
		    controller: 'ComplexModalController',
		});
		
	}
	
	// function for Datapicker
	$scope.datepicker = [];
	$scope.openDatepicker = function($event, datepicker) {
		$event.preventDefault();
		$event.stopPropagation();
		
		$scope.datepicker[datepicker] = {'open' : true};
	};

	
}]).controller('ComplexModalController', ['$scope', 'Restangular', function($scope, Restangular) {
	
	$scope.Projectleiders = Restangular.all('concept/Person/atoms').getList().$object;
	
	$scope.do = function(){
		console.log('click');
	}

}]);

