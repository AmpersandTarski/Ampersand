angular.module('AmpersandApp').service('ResourceService', function($scope, $rootScope, $localStorage, $timeout, Restangular){
    // http://blog.thoughtram.io/angular/2015/07/07/service-vs-factory-once-and-for-all.html
    
    let updatedResources = []; // contains list with updated resource objects in this interface. Used to check if there are uncommmitted changes (patches in cache)
    
    return {
        checkRequired : function(){ 
            updatedResources.reduce(function(prev, item, index, arr){
                return prev || item._patchesCache_.length;
            }, false);
        },
        
        emptyUpdatedResources : function(){
            updatedResources = [];
        },
        
        addPatches : function(resource, patches){
            if(!Array.isArray(resource._patchesCache_)) resource._patchesCache_) = [];
            
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
            if(!Array.isArray(resource._loading_)) resource._loading_) = []; // list with promises
            
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
                        $rootScope.addError('Failed to save resource: ' + reason);
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
            $rootScope.updateNotifications(response.notifications);
            
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
            resource._status_ = { 'warning' : false
                                , 'danger'  : false
                                , 'default' : false
                                , 'success' : false
                                };
            // Set status property
            resource._status_[status] = true;
        },
        
        pendingPromises : function(resource){
            if(!Array.isArray(resource._loading_)) return false; // empty array contains no pending promises
            
            return resource._loading_.some(function(val){
                return val.$$state.status == 0; // promise status: 0 -> pending, 1 -> resolved, 2 -> rejected
            });
        }
    };
    
});