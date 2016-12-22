angular.module('AmpersandApp').service('ResourceService', function($localStorage, $timeout, $location, Restangular, NotificationService){
    // http://blog.thoughtram.io/angular/2015/07/07/service-vs-factory-once-and-for-all.html
    
    let updatedResources = []; // contains list with updated resource objects in this interface. Used to check if there are uncommmitted changes (patches in cache)
    
    let ResourceService = {
        // Function to get (GET) a resource
        getResource : function(resource, ifc, callingObj){
            if(!Array.isArray(callingObj._loading_)) callingObj._loading_ = []; // list with promises
            
            callingObj._loading_.push(
                Restangular.one(resource._path_ + '/' + ifc)
                .get()
                .then(
                    function(data){
                        data = data.plain();
                        if($.isEmptyObject(data)) NotificationService.addInfo('No results found');
                        else if(resource[ifc] === null) resource[ifc] = data;
                        else angular.extend(resource[ifc], data);
                        
                        ResourceService.initResourceMetaData(resource);
                    }
                )
            );
        },
        
        // Function to cancel edits and reset resource data
        cancelResource : function(resource){
            if(!Array.isArray(resource._loading_)) resource._loading_ = []; // list with promises
            
            resource._loading_.push(
                Restangular.one(resource._path_)
                .get()
                .then(
                    function(data){
                        data = data.plain();
                        if($.isEmptyObject(data)) NotificationService.addInfo('No results found');
                        else angular.extend(resource, data);
                        
                        // Update visual feedback (notifications and buttons)
                        NotificationService.getNotifications();
                        ResourceService.initResourceMetaData(resource);
                    }
                )
            );
        },
        
        // Function to create (POST) a new resource
        createResource : function(resource, ifc, callingObj, prepend){
            if(prepend === 'undefined') prepend = false;
            if(!Array.isArray(callingObj._loading_)) callingObj._loading_ = []; // list with promises
            
            callingObj._loading_.push(
                Restangular.one(resource._path_).all(ifc)
                .post({}, {})
                .then(
                    function(data){
                        data = data.plain();
                        // Update visual feedback (notifications and buttons)
                        ResourceService.processResponse(callingObj, data);
                        
                        // Add new resource to ifc
                        if(Array.isArray(resource[ifc])){ // non-uni = list
                            if(prepend) resource[ifc].unshift(data.content);
                            else resource[ifc].push(data.content);
                        }else{ // uni = object
                            resource[ifc] = data.content;
                        }
                        
                        if(resource._isRoot_ && resource._id_ == '_NEW') $location.url('/' + ifc + '/'+ data.content._id_, false);
                    }
                )
            );
        },
        
        // Function to remove a resource from an interface (list)
        removeResource : function(parent, ifc, resource, patchResource){
            // Adapt js model
            if(Array.isArray(parent[ifc])) parent[ifc].splice(parent[ifc].indexOf(resource), 1); // non-uni = list
            else parent[ifc] = null; // uni = object
            
            // Construct patch(es)
            if(typeof patchResource === 'undefined') patchResource = resource;
            pathLength = patchResource._path_.length;
            path = resource._path_.substring(pathLength);
            patches = [{ op : 'remove', path : path}];
            
            // Patch!
            ResourceService.addPatches(patchResource, patches);
        },
        
        // Function to delete a resource
        deleteResource : function(parent, ifc, resource){
            if(!Array.isArray(resource._loading_)) resource._loading_ = []; // list with promises
            
            if(confirm('Are you sure?')){
                resource._loading_.push(
                    Restangular.one(resource._path_)
                    .remove({})
                    .then(
                        function(data){
                            data = data.plain();
                            // Update visual feedback (notifications and buttons)
                            NotificationService.updateNotifications(data.notifications);
                            
                            // Remove resource from ifc
                            if(Array.isArray(parent[ifc])) parent[ifc].splice(parent[ifc].indexOf(resource), 1); // non-uni = list
                            else parent[ifc] = null; // uni = object
                        }
                    )
                );
            }
        },
        // Function to save an ifc that is not a list
        saveItem : function(resource, ifc, patchResource){
            if(typeof resource[ifc] === 'undefined' || resource[ifc] === '') value = null;
            else value = resource[ifc];
            
            // Construct patch(es)
            if(typeof patchResource === 'undefined') patchResource = resource;
            pathLength = patchResource._path_.length;
            path = resource._path_.substring(pathLength) + '/' + ifc;
            patches = [{ op : 'replace', path : path, value : value}];
            
            ResourceService.addPatches(patchResource, patches);
        },
        
        // Function to add an item to an interface list
        addItem : function(resource, ifc, selected, patchResource){
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
        },
        
        // Function to remove an item from an interface list
        removeItem : function(resource, ifc, index, patchResource){
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
        },
        
        checkRequired : function(){ 
            updatedResources.reduce(function(prev, item, index, arr){
                return prev || item._patchesCache_.length;
            }, false);
        },
        
        emptyUpdatedResources : function(){
            updatedResources = [];
        },
        
        addPatches : function(resource, patches){
            if(!Array.isArray(resource._patchesCache_)) resource._patchesCache_ = [];
            
            // Add new patches to resource
            resource._patchesCache_ = resource._patchesCache_.concat(patches);
            
            // Add resource to updatedResources
            if(updatedResources.indexOf(resource) === -1) updatedResources.push(resource);
            
            // Save if autoSave is enabled
            if($localStorage.switchAutoSave) ResourceService.saveResource(resource);
            else {
                // Update visual feedback
                ResourceService.setResourceStatus(resource, 'warning');
                resource._showButtons_ = {'save' : true, 'cancel' : true};
            }
        },
        
        saveResource : function(resource){
            if(!Array.isArray(resource._loading_)) resource._loading_ = []; // list with promises
            
            resource._loading_.push(
                Restangular.one(resource._path_)
                .patch(resource._patchesCache_, {})
                .then(
                    function(data) {
                        data = data.plain();
                        // Update resource data
                        if(resource._isRoot_) resource = data.content;
                        else resource = angular.extend(resource, data.content);
                        
                        // Update visual feedback (notifications and buttons)
                        ResourceService.processResponse(resource, data);
                    }
                )
            );
        },
        
        // Init/reset resource meta data
        initResourceMetaData : function(resource){
            resource._showButtons_ = {'save' : false, 'cancel' : false};
            resource._patchesCache = [];
            ResourceService.setResourceStatus(resource, 'default');
        },
        
        // Process response: i.e. set resource buttons and status
        processResponse : function(resource, response){
            NotificationService.updateNotifications(response.notifications);
            
            if(response.invariantRulesHold){
                resource._showButtons_ = {'save' : false, 'cancel' : false};
                resource._patchesCache_ = []; // empty patches cache
                ResourceService.setResourceStatus(resource, 'success');
                
                // After 3 seconds, reset status to default
                $timeout(function(){
                    ResourceService.setResourceStatus(resource, 'default');
                }, 3000);
            }else{
                resource._showButtons_ = {'save' : false, 'cancel' : true};
                ResourceService.setResourceStatus(resource, 'danger');
            }
        },
        
        setResourceStatus : function(resource, status){
            // Reset all status properties
            resource._status_ = { 'warning' : false,
                                  'danger'  : false,
                                  'default' : false,
                                  'success' : false
                                };
            // Set status property
            resource._status_[status] = true;
        },
        
        pendingPromises : function(resource){
            if(!Array.isArray(resource._loading_)) return false; // empty array contains no pending promises
            
            return resource._loading_.some(function(val){
                return val.$$state.status === 0; // promise status: 0 -> pending, 1 -> resolved, 2 -> rejected
            });
        }
    };
    
    return ResourceService;
    
});