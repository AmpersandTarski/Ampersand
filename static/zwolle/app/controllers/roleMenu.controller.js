angular.module('AmpersandApp').controller('RoleMenuController', function ($scope, $rootScope, RoleService) {
    $scope.toggleRole = function(roleId, set){
        RoleService.toggleRole(roleId, set);
        $rootScope.setActiveRoles();
    };
});