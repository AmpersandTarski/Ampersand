angular.module('AmpersandApp')
.controller('NavigationBarController', function ($scope, $route, Restangular, $localStorage, $sessionStorage, $timeout, $location, NotificationService, RoleService, NavigationBarService) {
    
    $scope.$storage = $localStorage;
    $scope.$sessionStorage = $sessionStorage;
    $scope.loadingNavBar = [];
    $scope.navbar = NavigationBarService.navbar;
    
    $scope.reload = function(){
        $scope.loadingNavBar = [];
        $scope.loadingNavBar.push(NavigationBarService.refreshNavBar());
        $route.reload();
    };

    $scope.toggleRole = function(roleId, set){
        RoleService.toggleRole(roleId, set);
        $scope.loadingNavBar = [];
        $scope.loadingNavBar.push(
            RoleService.setActiveRoles()
            .then(function(data){
                NavigationBarService.refreshNavBar();
            })
        );
    };

    $scope.checkAllRules = NotificationService.checkAllRules;

    $scope.createNewResource = function(resourceType, openWithIfc){
        Restangular.one('resource').all(resourceType)
        .post({}, {})
        .then(
            function(data){
                // Jumps to interface and requests newly created resource
                $location.url(openWithIfc + '/' + data._id_);
            }
        );
    };
    
    $scope.resetSettings = function(){
        // all off
        angular.forEach($localStorage.notificationPrefs, 
            function(value, index, obj){
                obj[index] = false;
            }
        );
        $localStorage.switchAutoSave = false;
        
        $timeout(function() {
            // reset to default        
            NavigationBarService.resetNotificationSettings();
            NavigationBarService.resetSwitchAutoSave();
        }, 500);
    };
    
    $scope.loadingNavBar.push(NavigationBarService.refreshNavBar());
});
