app = angular.module('AmpersandApp')
.config(function($routeProvider) {
    $routeProvider
        // default start page
        .when('/ext/Login', {
            controller : 'LoginExtLoginController',
            templateUrl : 'extensions/OAuthLogin/ui/views/Login.html',
            interfaceLabel : 'Login'
        });
});
// Add Login module to dependency list
app.requires[app.requires.length] = 'LoginModule'; // add LoginModule to dependencies

// LoginModule declaration
angular.module('LoginModule', ['ngRoute', 'restangular'])
.controller('LoginExtLoginController', function($scope, Restangular, NotificationService){
    Restangular.one('oauthlogin/login').get().then(
        function(data){ // on success
            data = data.plain();
            $scope.idps = data.identityProviders;
            NotificationService.updateNotifications(data.notifications);
        }
    );
}).controller('LoginExtLogoutController', function($scope, Restangular, $location, NotificationService, NavigationBarService){
    $scope.logout = function(){
        Restangular.one('oauthlogin/logout').get().then(
            function(data){ // success
                data = data.plain();
                NotificationService.updateNotifications(data.notifications);
                NavigationBarService.refreshNavBar();
                $location.path('/'); // goto home
            }
        );
    };
});