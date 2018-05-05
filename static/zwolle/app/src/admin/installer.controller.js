angular.module('AmpersandApp')
.controller('InstallerController', function ($scope, Restangular, NotificationService, RoleService, NavigationBarService) {
    $scope.installing = false;
    $scope.installed = false;
    
    $scope.install = function(defPop){
        $scope.installing = true;
        $scope.installed = false;
        Restangular.one('admin/installer').get({defaultPop : defPop}).then(function(data) {
            data = data.plain();
            NotificationService.updateNotifications(data);
            NavigationBarService.refreshNavBar();
            
            // deactive all roles
            RoleService.deactivateAllRoles();
            
            $scope.installing = false;
            $scope.installed = true;
        }, function(){
            $scope.installing = false;
            $scope.installed = false;
        });
    };
});
