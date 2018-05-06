angular.module('AmpersandApp')
.service('ResourceService', function($localStorage, $timeout, $location, Restangular, NotificationService, $q){
    // http://blog.thoughtram.io/angular/2015/07/07/service-vs-factory-once-and-for-all.html
    
    let updatedResources = []; // contains list with updated resource objects in this interface. Used to check if there are uncommmitted changes (patches in cache)
    
    let ResourceService = {
        /**
         * Get resource data given a certain interface (ifc)
         * 
         * @param {Object} resource
         * @param {string} ifc
         * @param {Object} callingObj will be used for loading indicator
         * @returns {Promise}
         */
        getResource : function(resource, ifc, callingObj){
            promise = Restangular
            .one(resource._path_ + '/' + ifc)
            .get()
            .then(function(data){
                try {
                    data = data.plain();
                }catch(error){}
                if($.isEmptyObject(data)) NotificationService.addInfo('No results found');
                else if(resource[ifc] === null || Array.isArray(resource[ifc])) resource[ifc] = data;
                else angular.extend(resource[ifc], data);
                
                ResourceService.initResourceMetaData(resource);

                return resource;
            });
            
            // Add promise to loading list
            return ResourceService.addPromiseToResourceLoadingList(callingObj, promise);
        },

        /**
         * Patch the given resource by calling the API and sending the list of stored patches 
         * 
         * @param {Object} resource
         * @param {bool} forceSave
         * @returns {Promise}
         */
        patchResource : function(resource, forceSave){

            // Save if autoSave is enabled
            if($localStorage.autoSave || forceSave) {
                promise = Restangular
                .one(resource._path_)
                .patch(resource._patchesCache_, {})
                .then(function(data) {
                    data = data.plain();
                    
                    // Update visual feedback (notifications and buttons)
                    ResourceService.processResponse(resource, data);

                    // Update resource data if committed
                    if(data.isCommitted) {
                        if(resource._isRoot_ && data.navTo == null) resource.get(); // if directed to other page (data.navTo), refresh of data is not needed
                        else resource = angular.extend(resource, data.content);
                        return {resource : resource, saved: true};
                    } else {
                        return {resource : resource, saved: false};
                    }
                });

                // Add promise to loading list
                return ResourceService.addPromiseToResourceLoadingList(resource, promise);
            } else {
                // Update visual feedback
                ResourceService.setResourceStatus(resource, 'warning');
                resource._showButtons_ = {'save' : true, 'cancel' : true};
                return $q.resolve({resource : resource, saved : false});
            }
        },
        
        /**
         * Cancel unsaved edits and get resource data
         * 
         * @param {Object} resource
         * @returns {Promise}
         */
        cancelResource : function(resource){
            promise = Restangular
            .one(resource._path_)
            .get()
            .then(function(data){
                data = data.plain();
                if($.isEmptyObject(data)) NotificationService.addInfo('No results found');
                else angular.extend(resource, data);
                
                // Update visual feedback (notifications and buttons)
                NotificationService.getNotifications();
                ResourceService.initResourceMetaData(resource);

                return resource;
            });
            
            // Add promise to loading list
            return ResourceService.addPromiseToResourceLoadingList(resource, promise);
        },
        
        /**
         * Create (POST) a new resource to a certain interface list
         * 
         * @param {Object} resource
         * @param {string} ifc
         * @param {Object} callingObj will be used for loading indicator
         * @param {int} insertAtIndex
         * @returns {Promise}
         */
        createResource : function(resource, ifc, callingObj, insertAtIndex){
            promise = Restangular
            .one(resource._path_).all(ifc)
            .post({}, {})
            .then(function(data){
                data = data.plain();
                newResource = data.content;

                // Update visual feedback (notifications and buttons)
                ResourceService.processResponse(newResource, data);
                
                // Add new resource to ifc
                if(Array.isArray(resource[ifc])){ // non-uni = list
                    if(insertAtIndex === 'undefined') insertAtIndex = resource[ifc].length; // append by default
                    resource[ifc].splice(insertAtIndex, 0, newResource);
                }else{ // uni = object
                    resource[ifc] = newResource;
                }
                
                if(resource._isRoot_ && resource._id_ == '_NEW') $location.url('/' + ifc + '/'+ newResource._id_, false);

                return newResource;
            });

            // Add promise to loading list
            return ResourceService.addPromiseToResourceLoadingList(callingObj, promise);
        },
        
        /**
         * Remove a resource from a certain interface list
         * 
         * @param {Object} parent
         * @param {string} ifc
         * @param {Object} resource
         * @param {Object} patchResource
         * @returns {Promise}
         */
        removeResource : function(parent, ifc, resource, patchResource){
            // Construct patch(es)
            ResourceService.addPatch('remove', resource, patchResource);

            // Execute patch
            return ResourceService
            .patchResource(patchResource)
            .then(function(data){
                // Adapt js model
                if(!data.saved) {
                    if(Array.isArray(parent[ifc])) parent[ifc].splice(parent[ifc].indexOf(resource), 1); // non-uni = list
                    else parent[ifc] = null; // uni = object
                }
            });
        },
        
        /**
         * Delete a resource
         * 
         * @param {Object} parent
         * @param {string} ifc
         * @param {Object} resource to delete
         * @returns {Promise}
         */
        deleteResource : function(parent, ifc, resource){
            if(confirm('Are you sure?')){
                promise = Restangular
                .one(resource._path_)
                .remove({})
                .then(function(data){
                    data = data.plain();
                    // Update visual feedback (notifications and buttons)
                    NotificationService.updateNotifications(data.notifications);
                    
                    // Remove resource from ifc
                    if(Array.isArray(parent[ifc])) parent[ifc].splice(parent[ifc].indexOf(resource), 1); // non-uni = list
                    else parent[ifc] = null; // uni = object

                    return parent;
                });

                // Add promise to loading list
                return ResourceService.addPromiseToResourceLoadingList(resource, promise);
            }
        },

        /**
         * Save/patch a changed attribute
         * 
         * @param {Object} resource
         * @param {string} ifc
         * @param {Object} patchResource
         * @returns {Promise}
         */
        saveItem : function(resource, ifc, patchResource){
            // Construct patch(es)
            if(typeof resource[ifc] === 'undefined' || resource[ifc] === '') {
                value = null;
            } else {
                value = resource[ifc];
            }
            ResourceService.addPatch('replace', resource, patchResource, ifc, value);

            // Register patch
            return ResourceService.patchResource(patchResource);
        },
        
        /**
         * Add an item to an interface list
         * 
         * @param {Object} resource
         * @param {string} ifc
         * @param {Object} selected item to add to the list
         * @param {Object} patchResource
         * @returns {Promise}
         */
        addItem : function(resource, ifc, selected, patchResource){
            if(typeof selected.value === 'undefined') {
                //console.log('Value undefined');
                return $q.reject('Value undefined');
            } else if(selected.value === '') {
                //console.log('Empty value selected');
                return $q.reject('Empty value selected');
            } else if(!Array.isArray(resource[ifc])) {
                //console.log('Error: trying to add item to non-array');
                return $q.reject('Error: trying to add item to non-array');
            } else{
                // Adapt in js model
                resource[ifc].push(selected.value);
                
                // Construct patch(es)
                ResourceService.addPatch('add', resource, patchResource, ifc, selected.value);
                return ResourceService.patchResource(patchResource)
                .then(function(data){
                    // Reset selected value
                    delete(selected.value);
                    return data;
                });
            }
        },
        
        /**
         * Remove an item from an interface list
         * 
         * @param {Object} resource
         * @param {string} ifc
         * @param {int} index
         * @param {Object} patchResource
         * @returns {Promise}
         */
        removeItem : function(resource, ifc, index, patchResource){
            // Construct patch(es)
            value = resource[ifc][index];
            ResourceService.addPatch('remove', resource, patchResource, ifc, value);
            
            // Adapt js model
            resource[ifc].splice(index, 1);

            return ResourceService.patchResource(patchResource);
        },
        
        /**
         * Construct, add and return patch object (with attributes 'op', 'path' and 'value')
         * 
         * @param {string} operation choose from 'add', 'remove' or 'replace'
         * @param {Object} resource
         * @param {Object} patchResource resource to add patch to
         * @param {string} ifc
         * @param {string} value
         * @returns {Object}
         */
        addPatch : function(operation, resource, patchResource, ifc, value){
            if(typeof patchResource === 'undefined') patchResource = resource;
            pathLength = patchResource._path_.length;
            
            path = resource._path_.substring(pathLength);
            if(typeof ifc !== 'undefined') path = path + '/' + ifc;
            
            if(typeof value === 'undefined') patch = { op : operation, path : path};
            else patch = { op : operation, path : path, value : value};

            // Add new patch to patchResource
            if(!Array.isArray(patchResource._patchesCache_)) patchResource._patchesCache_ = [];
            patchResource._patchesCache_.push(patch);

            // Add resource to updatedResources
            if(updatedResources.indexOf(patchResource) === -1) updatedResources.push(patchResource);

            return patch;
        },

        /**
         * Returns if there are unsaved changes (i.e. patches that are not yet sent to the API)
         * 
         * @returns {bool}
         */
        checkRequired : function(){ 
            return updatedResources.reduce(function(prev, item, index, arr){
                return prev || item._patchesCache_.length;
            }, false);
        },
        
        /**
         * Clear list of updated resources
         */
        emptyUpdatedResources : function(){
            updatedResources = [];
        },
        
        /**
         * Init/reset resource meta data
         * 
         * @param {Object} resource
         */
        initResourceMetaData : function(resource){
            resource._showButtons_ = {'save' : false, 'cancel' : false};
            resource._patchesCache_ = [];
            ResourceService.setResourceStatus(resource, 'default');
        },
        
        /**
         * Process response: i.e. set resource buttons and status
         * 
         * @param {Object} resource
         * @param {Object} response from API
         * @returns {Object}
         */
        processResponse : function(resource, response){
            NotificationService.updateNotifications(response.notifications);
            
            if(response.isCommitted){
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

            return resource;
        },
        
        /**
         * Set resource status meta data
         * 
         * @param {Object} resource
         * @param {string} status choose from 'warning', 'danger', 'success' or 'default'
         * @returns {Object}
         */
        setResourceStatus : function(resource, status){
            // Reset all status properties
            resource._status_ = { 'warning' : false,
                                  'danger'  : false,
                                  'default' : false,
                                  'success' : false
                                };
            // Set status property
            resource._status_[status] = true;
            
            return resource;
        },
        
        /**
         * Returns if resource has pending promises
         * 
         * @param {Object} resource
         * @returns {bool}
         */
        pendingPromises : function(resource){
            if(!Array.isArray(resource._loading_)) return false; // empty array contains no pending promises
            
            return resource._loading_.some(function(val){
                return val.$$state.status === 0; // promise status: 0 -> pending, 1 -> resolved, 2 -> rejected
            });
        },

        /**
         * @param {Object} resource
         * @param {Promise} promise
         * @returns {Promise}
         */
        addPromiseToResourceLoadingList : function(resource, promise){
            if(!Array.isArray(resource._loading_)) resource._loading_ = [];
            resource._loading_.push(promise);
            resource._isLoading_ = true;

            return promise.finally(function(){
                if(!ResourceService.pendingPromises(resource)) {
                    resource._isLoading_ = false;
                    resource._loading_ = [];
                }
            });
        }
    };
    
    return ResourceService;
});
