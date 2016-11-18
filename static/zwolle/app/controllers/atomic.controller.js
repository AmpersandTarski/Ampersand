angular.module('AmpersandApp').controller('AtomicController', function($scope, ResourceService){
    
    /*
     * Object to temporary store value/resourceId to add to list
     * Value/resourceId is stored as property of 'selected' obj. This is needed to pass it around by reference
     */
    $scope.selected = {};
    
    // Function to save an ifc that is not a list
    $scope.saveItem = function(resource, ifc, patchResource){
        if(typeof resource[ifc] === 'undefined' || resource[ifc] === '') value = null;
        else value = resource[ifc];
        
        // Construct patch(es)
        if(typeof patchResource === 'undefined') patchResource = resource;
        pathLength = patchResource._path_.length;
        path = resource._path_.substring(pathLength) + '/' + ifc;
        patches = [{ op : 'replace', path : path, value : value}];
        
        ResourceService.addPatches(patchResource, patches);
    };
    
    // Function to add an item to an interface list
    $scope.addItem = function(resource, ifc, selected, patchResource){
        if(typeof selected.value === 'undefined') console.log('Value undefined');
        else if(selected.value === '') console.log('Empty value selected');
        else if(!Array.isArray(resource[ifc])) console.log('Error: trying to add item to non-array');
        else{
            // Adapt in js model
            resource[ifc].push(selected.value);
            
            // Construct patch(es)
            if(typeof patchResource === 'undefined') patchResource = resource;
            pathLength = patchResource._path_.length;
            path = resource._path_.substring(pathLength) + '/' + ifc;
            patches = [{ op : 'add', path : path, value : selected.value}];
            
            // Reset selected value
            delete(selected.value);
            
            // Patch!
            ResourceService.addPatches(patchResource, patches);
        }
    };
    
    $scope.removeItem = function(resource, ifc, index, patchResource){
        // Adapt js model
        value = resource[ifc][index];
        resource[ifc].splice(index, 1);
        
        // Construct patch(es)
        if(typeof patchResource === 'undefined') patchResource = resource;
        pathLength = patchResource._path_.length;
        path = resource._path_.substring(pathLength) + '/' + ifc;
        patches = [{ op : 'remove', path : path, value: value}];
        
        // Patch!
        ResourceService.addPatches(patchResource, patches);
    };
    
    $scope.remove = ResourceService.removeResource; // function(ifc, resource, patchResource)
    
    $scope.delete = ResourceService.deleteResource; // function(ifc, resource)
});