angular.module('AmpersandApp').controller('static_atomicController', function($scope){
    
    /*
     * Object to temporary store value/resourceId to add to list
     * Value/resourceId is stored as property of 'selected' obj. This is needed to pass it around by reference
     */
    $scope.selected = {};
    
    $scope.hasNoResults = false;
    
    // Regular function used by Atomic-OBJECT template
    $scope.typeaheadOnSelect = function ($item, $model, $label, resource, ifc, patchResource){
        if(typeof $item._id_ === 'undefined') console.log('Resource id undefined');
        else if($item._id_ === '') console.log('Empty resource id provided');
        else{
            selected = {value : $item._id_};
            if(Array.isArray(resource[ifc])) $scope.addItem(resource, ifc, selected, patchResource);
            else if(resource[ifc] === null){
                resource[ifc] = $item._id_;
                $scope.saveItem(resource, ifc, patchResource);
            }
            else console.log('Error: Property already set and/or not defined');
            
            $scope.hasNoResults = false;
        }
    };
    
    // Function to save ifc (not a list)
    $scope.saveItem = function(resource, ifc, patchResource){
        if(typeof resource[ifc] === 'undefined' || resource[ifc] === '') value = null;
        else value = resource[ifc];
        
        // Construct patch(es)
        if(typeof patchResource === 'undefined') patchResource = resource;
        pathLength = patchResource._path_.length;
        path = resource._path_.substring(pathLength) + '/' + ifc;
        patches = [{ op : 'replace', path : path, value : value}];
        
        addPatches(patchResource, patches);
    };
    
    // Function to add item to list
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
            addPatches(patchResource, patches);
        }
    };
    
    // Function to remove item from array
    $scope.removeItem = function(resource, ifc, key, patchResource){
        // Adapt js model
        value = resource[ifc][key];
        resource[ifc].splice(key, 1);
        
        // Construct patch(es)
        if(typeof patchResource === 'undefined') patchResource = resource;
        pathLength = patchResource._path_.length;
        path = resource._path_.substring(pathLength) + '/' + ifc;
        patches = [{ op : 'remove', path : path, value: value}];
        
        // Patch!
        addPatches(patchResource, patches);
    };
    
    // Function to remove an object from a certain interface (array) of a resource
    $scope.removeObject = function(resource, ifc, key, patchResource){
        // Adapt js model
        id = resource[ifc][key]['_id_'];
        resource[ifc].splice(key, 1);
        
        // Construct path
        if(typeof patchResource === 'undefined') patchResource = resource;
        pathLength = patchResource['_path_'].length;
        path = resource['_path_'].substring(pathLength) + '/' + ifc + '/' + id;
        
        // Construct patch
        patches = [{ op : 'remove', path : path}];
        
        // Patch!
        addPatches(patchResource, patches);
    };
    
    /*
     * Typeahead functionality
     * $scope.typeahead is initiated in static_interfaceController to be able to reuse typeahead data
     */
    $scope.getTypeahead = function(resourceType){
        // Only if not yet set
        if(typeof $scope.typeahead[resourceType] === 'undefined'){
            $scope.typeahead[resourceType] = Restangular.all('resources/' + resourceType).getList().$object;
        }
    };
}