angular.module('AmpersandApp').controller('AtomicController', function($scope, ResourceService){
    
    /*
     * Object to temporary store value/resourceId to add to list
     * Value/resourceId is stored as property of 'selected' obj. This is needed to pass it around by reference
     */
    $scope.selected = {};
    
    $scope.saveItem = ResourceService.saveItem; // function(resource, ifc, patchResource)
    
    $scope.addItem = ResourceService.addItem; // function(resource, ifc, selected, patchResource)
    
    $scope.removeItem = ResourceService.removeItem; // function(resource, ifc, index, patchResource)
    
    $scope.remove = ResourceService.removeResource; // function(ifc, resource, patchResource)
    
    $scope.delete = ResourceService.deleteResource; // function(ifc, resource)
});