angular.module('AmpersandApp').service('ResourceService', function($localStorage, $timeout, $location, Restangular, NotificationService){
    // http://blog.thoughtram.io/angular/2015/07/07/service-vs-factory-once-and-for-all.html
    
    let updatedResources = []; // contains list with updated resource objects in this interface. Used to check if there are uncommmitted changes (patches in cache)
    
    return {
        // Function to get (GET) a resource
        getResource : function(resource, ifc, callingObj){
            if(!Array.isArray(callingObj._loading_)) callingObj._loading_ = []; // list with promises
            
            callingObj._loading_.push(
                Restangular.one(resource._path_ + '/' + ifc)
                .get()
                .then(
                    function(data){
                        if($.isEmptyObject(data)) NotificationService.addInfo('No results found');
                        else angular.extend(resource[ifc], data);
                    }, function(reason){
                        NotificationService.addError('Failed to get resource: ' + reason);
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
                        if($.isEmptyObject(data)) NotificationService.addInfo('No results found');
                        else resource = data;
                        
                        // Update visual feedback (notifications and buttons)
                        NotificationService.getNotifications();
                        this.initResourceMetaData(resource);
                    }, function(reason){
                        NotificationService.addError('Failed to get resource: ' + reason);
                    }
                )
            );
        },
        
        // Function to create (POST) a new resource
        createResource : function(obj, ifc, callingObj, prepend){
            if(prepend === 'undefined') prepend = false;
            if(!Array.isArray(callingObj._loading_)) callingObj._loading_ = []; // list with promises
            
            callingObj._loading_.push(
                Restangular.one(obj._path_).all(ifc)
                .post({}, {})
                .then(
                    function(data){
                        // Update visual feedback (notifications and buttons)
                        this.processResponse(callingObj, data);
                        
                        // Add new resource to ifc
                        if(!Array.isArray(obj[ifc])){ // non-uni -> list
                            if(prepend) obj[ifc].unshift(data.content);
                            else obj[ifc].push(data.content);
                        }else{ // uni
                            obj[ifc] = data.content;
                        }
                        
                        if(obj._isRoot_ && obj._id_ == '_NEW') $location.url('/' + ifc + '/'+ data.content._id_, false);
                    }, function(reason){
                        NotificationService.addError('Failed to create resource: ' + reason);
                    }
                )
            );
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
                            // Update visual feedback (notifications and buttons)
                            NotificationService.updateNotifications(data.notifications);
                            
                            // Remove resource from ifc
                            if(Array.isArray(parent[ifc])) parent[ifc].splice(parent[ifc].indexOf(resource), 1); // non-uni -> list
                            else parent[ifc] = null; // uni
                        }, function(reason){
                            NotificationService.addError('Failed to delete resource: ' + reason);
                        }
                    )
                );
            }
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
            if($localStorage.switchAutoSave) this.saveResource(resource);
            else {
                // Update visual feedback
                this.setResourceStatus(resource, 'warning');
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
                        // Update resource data
                        if(resource._isRoot_) resource = data.content;
                        else resource = angular.extend(resource, data.content);
                        
                        // Update visual feedback (notifications and buttons)
                        this.processResponse(resource, data);
                    },function(reason){
                        NotificationService.addError('Failed to save resource: ' + reason);
                    }
                )
            );
        },
        
        // Init/reset resource meta data
        initResourceMetaData : function(resource){
            resource._showButtons_ = {'save' : false, 'cancel' : false};
            resource._patchesCache = [];
            this.setResourceStatus(resource, 'default');
        },
        
        // Process response: i.e. set resource buttons and status
        processResponse : function(resource, response){
            NotificationService.updateNotifications(response.notifications);
            
            if(response.invariantRulesHold){
                resource._showButtons_ = {'save' : false, 'cancel' : false};
                resource._patchesCache_ = []; // empty patches cache
                this.setResourceStatus(resource, 'success');
                
                // After 3 seconds, reset status to default
                $timeout(function(){
                    this.setResourceStatus(resource, 'default');
                }, 3000);
            }else{
                resource._showButtons_ = {'save' : false, 'cancel' : true};
                this.setResourceStatus(resource, 'danger');
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
    
});