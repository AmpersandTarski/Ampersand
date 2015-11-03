var app = angular.module('AmpersandApp');
app.requires[app.requires.length] = 'LoginModule'; // add ur.file module to dependencies

var LoginModule = angular.module('LoginModule', ['ngRoute', 'restangular']);

LoginModule.controller('LoginExtLoginController', function($scope, $rootScope, LoginRestangular){
	
	LoginRestangular.one('login').get().then(
		function(data){ // success
			$scope.idps = data.identityProviders;
			$rootScope.updateNotifications(data.notifications);
			
		}, function(){ // error
		
		}
	);
		
	
}).controller('LoginExtLogoutController', function($scope, $rootScope, LoginRestangular, $location){
	
	$scope.logout = function(){
		$rootScope.selectRole(0);
		$location.path('/'); // goto home
		
		LoginRestangular.one('logout').get().then(
			function(data){ // success
				
				$rootScope.updateNotifications(data.notifications);
				
			}, function(){ // error
				
			}
		);
	}
	
});

app.config(function($routeProvider) {
	$routeProvider
		// default start page
		.when('/ext/Login',
			{	controller: 'LoginExtLoginController'
			,	templateUrl: 'extensions/Login/ui/views/Login.html'
			,	interfaceLabel: 'Login'
			})
});

//Restangular service for the ExecEngine
LoginModule.factory('LoginRestangular', function(Restangular) {
	return Restangular.withConfig(function(RestangularConfigurer) {
		RestangularConfigurer.setBaseUrl('extensions/Login/api');
	});
});