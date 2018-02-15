angular.module('AmpersandApp').controller('NavigationBarController', function ($scope, $route, Restangular, $localStorage, $sessionStorage, $timeout, $location, NotificationService, RoleService, NavigationBarService) {
    
    $scope.$storage = $localStorage;
    $scope.$sessionStorage = $sessionStorage;
    $scope.defaultSettings = NavigationBarService.defaultSettings;
    $scope.loadingNavBar = [];
    $scope.navbar = NavigationBarService.navbar;
    
    $scope.reload = function(){
        $scope.loadingNavBar = [];
        $scope.loadingNavBar.push(NavigationBarService.refreshNavBar());
        $route.reload();
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
        angular.forEach($scope.$storage.notificationPrefs, 
            function(value, index, obj){
                obj[index] = false;
            }
        );
        $scope.$storage.switchAutoSave = false;
        
        $timeout(function() {
            // reset to default        
            $scope.resetNotificationSettings();
            $scope.resetSwitchAutoSave();
        }, 500);
    };
    
    $scope.resetNotificationSettings = function(){
        $scope.$storage.notificationPrefs = angular.extend($scope.$storage.notificationPrefs, $scope.defaultSettings.notifications);
    };
    
    $scope.resetSwitchAutoSave = function(){
        $scope.$storage.switchAutoSave = $scope.defaultSettings.switchAutoSave;
    };
    
    $scope.loadingNavBar.push(NavigationBarService.refreshNavBar());
}).directive('myNavbarResize', function ($window, $timeout, NavigationBarService) {
    return function (scope, element) {
        var w = angular.element($window);
        
        var resizeNavbar = function() {
            $timeout(function(){
                // moving ifc items from dropdown-menu to navbar itself
                while($('#navbar-interfaces').width() < ($('#navbar-wrapper').width() - $('#navbar-options').width()) &&
                        $('#navbar-interfaces-dropdown-menu').children().length > 0){
                    $("#navbar-interfaces-dropdown-menu").children().first().appendTo("#navbar-interfaces");
                }
                
                // moving ifc items from navbar to dropdown-menu
                while($('#navbar-interfaces').width() > ($('#navbar-wrapper').width() - $('#navbar-options').width())){
                    $("#navbar-interfaces").children().last().prependTo("#navbar-interfaces-dropdown-menu");
                    
                    // show/hide dropdown menu for more interfaces (must be inside loop, because it affects the width of the navbar
                    $('#navbar-interfaces-dropdown').toggleClass('hidden', $('#navbar-interfaces-dropdown-menu').children().length <= 0);
                }
                
                // show/hide dropdown menu when possible
                $('#navbar-interfaces-dropdown').toggleClass('hidden', $('#navbar-interfaces-dropdown-menu').children().length <= 0);
            });
        };
        
        // watch navbar
        scope.$watch('NavigationBarService.navbar', function() {
            resizeNavbar();
        });
        
        // when window size gets changed
        w.bind('resize', function () {        
            resizeNavbar();
        });
        
        // when page loads
        resizeNavbar();
    };
});