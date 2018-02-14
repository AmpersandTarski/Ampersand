angular.module('AmpersandApp').controller('RoleMenuController', function ($scope, RoleService, NavigationBarService) {
    $scope.toggleRole = function(roleId, set){
        RoleService.toggleRole(roleId, set);
        NavigationBarService.promises.push(
            RoleService.setActiveRoles()
            .then(function(data){
                NavigationBarService.refreshNavBar();
            })
        );
    };
});