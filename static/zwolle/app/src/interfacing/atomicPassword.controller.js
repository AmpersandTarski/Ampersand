angular.module('AmpersandApp')
.controller('AtomicPasswordController', function($scope, ResourceService){
    // Silently add patch. Change is not autosaved, because browser autofill can otherwise infinite loops 
    $scope.patchPasswordField = function(resource, ifc, patchResource) {
        if(typeof resource[ifc] === 'undefined' || resource[ifc] === '') {
            value = null;
        } else {
            value = resource[ifc];
            ResourceService.addPatch('replace', resource, patchResource, ifc, value);
        }
    }
});