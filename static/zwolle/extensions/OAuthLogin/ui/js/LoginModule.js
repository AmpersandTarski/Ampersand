angular.module('AmpersandApp')
.config(function($routeProvider) {
	$routeProvider
		// default start page
		.when('/ext/Login',
			{	controller: 'LoginExtLoginController'
			,	templateUrl: 'extensions/OAuthLogin/ui/views/Login.html'
			,	interfaceLabel: 'Login'
			})
});

// Add Login module to dependency list
app = angular.module('AmpersandApp');
app.requires[app.requires.length] = 'LoginModule'; // add ur.file module to dependencies

// LoginModule declaration
angular.module('LoginModule', ['ngRoute', 'restangular'])
.controller('LoginExtLoginController', function($scope, $rootScope, Restangular, NotificationService){
	
	Restangular.one('oauthlogin/login').get().then(
		function(data){ // success
            data = data.plain();
			$scope.idps = data.identityProviders;
			NotificationService.updateNotifications(data.notifications);
			
		}, function(){ // error
		
		}
	);
	
}).controller('LoginExtLogoutController', function($scope, $rootScope, Restangular, $location){
	
	$scope.logout = function(){
		Restangular.one('oauthlogin/logout').get().then(
			function(data){ // success
				data = data.plain();
				NotificationService.updateNotifications(data.notifications);
				$rootScope.deactivateAllRoles();
				$location.path('/'); // goto home
				
			}, function(){ // error
				
			}
		);
	}
});