angular.module('AmpersandApp').service('RoleService', function($sessionStorage){
    
    /*
     * Available roles are registered in $sessionStorage.sessionRoles
     * A role has the following attributes: id, label, active
     */
    
    RoleService = {
        selectRole : function(roleId){
            RoleService.toggleRole(roleId, true);
        },
        
        selectRoleByLabel : function (roleLabel){
            angular.forEach($sessionStorage.sessionRoles, function(role) {
                if(role.label == roleLabel) return RoleService.selectRole(role.id);
            });
        },
        
        toggleRole : function(roleId, set){
            angular.forEach($sessionStorage.sessionRoles, function(role) {
                if (role.id == roleId) {
                    if(set === undefined) role.active = !role.active;
                    else role.active = set;
                }
            });
        },
        
        getActiveRoleIds : function(){
            var roleIds = [];
            angular.forEach($sessionStorage.sessionRoles, function(role) {
                if (role.active === true) {
                    roleIds.push(role.id);
                }
            });
            return roleIds;
        },
        
        deactivateAllRoles : function(){
            angular.forEach($sessionStorage.sessionRoles, function(role) {
                role.active = false;
            });
        }
    };
    
    return RoleService;
});