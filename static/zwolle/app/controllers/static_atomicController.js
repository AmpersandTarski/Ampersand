angular.module('AmpersandApp').controller('static_atomicController', function($scope){
    
    /*
     * Object to temporary store value/resourceId to add to list
     * Value/resourceId is stored as property of 'selected' obj. This is needed to pass it around by reference
     */
    $scope.selected = {};
    
    $scope.hasNoResults = false;
    
    // Regular function used by Atomic-OBJECT template
    $scope.typeaheadOnSelect = function ($item, $model, $label, obj, property, patchResource){
        $scope.addObject(obj, property, $item, patchResource);
        $scope.hasNoResults = false;
    };
    
    // Function to save ifc (not a list)
    $scope.saveItem = function(resource, ifc, patchResource){
        if(typeof resource[ifc] === 'undefined' || resource[ifc] === '') value = null;
        else value = resource[ifc];
        
        // Construct patch(es)
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
        pathLength = patchResource._path_.length;
        path = resource._path_.substring(pathLength) + '/' + ifc;
        patches = [{ op : 'remove', path : path, value: value}];
        
        // Patch!
        addPatches(patchResource, patches);
    };
    
    // Function to add an object to a certain interface (array) of a resource
    $scope.addObject = function(resource, ifc, obj, patchResource){
        // If patchResource is undefined, the patchResource equals the resource
        if(typeof patchResource === 'undefined'){
            patchResource = resource
        }
        
        if(typeof obj['_id_'] === 'undefined' || obj['_id_'] == ''){
            console.log('Selected object id is undefined');
        }else{
            try {
                obj = obj.plain(); // plain is Restangular function
            }catch(e){} // when plain() does not exists (i.e. object is not restangular object)
            
            // Adapt js model
            if(resource[ifc] === null) resource[ifc] = obj;
            else if(Array.isArray(resource[ifc])) resource[ifc].push(obj);
            else console.log('Cannot add object. Resource[ifc] already set and/or not defined');
            
            // Construct path
            pathLength = patchResource['_path_'].length;
            path = resource['_path_'].substring(pathLength) + '/' + ifc;
            
            // Construct patch
            patches = [{ op : 'add', path : path, value : obj['_id_']}];
            
            // Patch!
            addPatches(patchResource, patches);
        }
    };
    
    // Function to remove an object from a certain interface (array) of a resource
    $scope.removeObject = function(resource, ifc, key, patchResource){
        // Adapt js model
        id = resource[ifc][key]['_id_'];
        resource[ifc].splice(key, 1);
        
        // Construct path
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