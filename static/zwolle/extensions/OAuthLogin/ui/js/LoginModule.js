var app = angular.module('AmpersandApp');
app.requires[app.requires.length] = 'LoginModule'; // add ur.file module to dependencies

var LoginModule = angular.module('LoginModule', ['ngRoute', 'restangular']);

LoginModule.controller('LoginExtLoginController', function($scope, $rootScope, Restangular){
	
	Restangular.one('oauthlogin/login').get().then(
		function(data){ // success
			$scope.idps = data.identityProviders;
			$rootScope.updateNotifications(data.notifications);
			
		}, function(){ // error
		
		}
	);
	
}).controller('LoginExtLogoutController', function($scope, $rootScope, Restangular, $location){
	
	$scope.logout = function(){
		Restangular.one('oauthlogin/logout').get().then(
			function(data){ // success
				
				$rootScope.updateNotifications(data.notifications);
				$rootScope.deactivateAllRoles();
				$location.path('/'); // goto home
				
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
			,	templateUrl: 'extensions/OAuthLogin/ui/views/Login.html'
			,	interfaceLabel: 'Login'
			})
});