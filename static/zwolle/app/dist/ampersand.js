// when using minified angular modules, use module('myApp', []).controller('MyController', ['myService', function (myService) { ...
angular.module('AmpersandApp', ['ngResource', 'ngRoute', 'ngSanitize', 'restangular', 'ui.bootstrap', 'uiSwitch', 'cgBusy', 'siTable', 'ui.codemirror', 'ngStorage', 'angularFileUpload', 'ui.bootstrap.datetimepicker', 'hc.marked'])
.config(["$routeProvider", "$locationProvider", function($routeProvider, $locationProvider) {
    $routeProvider
        // default start page
        .when('/', { 
            controller : '',
            templateUrl : 'app/src/shared/home.html',
            interfaceLabel : 'Home'
            })
        // installer page
        .when('/admin/installer', {
            controller : 'InstallerController',
            templateUrl : 'app/src/admin/installer.html',
            interfaceLabel : 'Installer'
            })
        .when('/404', {
            templateUrl: 'app/src/shared/404.html',
            interfaceLabel: '404'
            })
        .otherwise({redirectTo: '/404'});
    
    $locationProvider.hashPrefix(''); // see: https://stackoverflow.com/questions/41211875/angularjs-1-6-0-latest-now-routes-not-working
}]).config(["RestangularProvider", function(RestangularProvider) {
    
    RestangularProvider.setBaseUrl('api/v1'); // Generate: path to API folder
    RestangularProvider.setDefaultHeaders({"Content-Type": "application/json"});
    // RestangularProvider.setPlainByDefault(true); available from Restangular v1.5.3
    
}]).run(["Restangular", "$rootScope", "$location", "$route", "NotificationService", "RoleService", "NavigationBarService", function(Restangular, $rootScope, $location, $route, NotificationService, RoleService, NavigationBarService){

    Restangular.addFullRequestInterceptor(function(element, operation, what, url, headers, params){
        //params.navIfc = true;
        //params.metaData = true;
        return params;
    });
    
    Restangular.addResponseInterceptor(function(data, operation, what, url, response, deferred){
        if(operation != 'get' && operation != 'getList' && data.sessionRefreshAdvice) NavigationBarService.refreshNavBar();
		if((data || {}).navTo != null) $location.url(data.navTo);
        
        return data;
    });
    
    Restangular.setErrorInterceptor(function(response, deferred, responseHandler) {
        // 401: Unauthorized
        if(response.status == 401) {
            RoleService.deactivateAllRoles();
            $location.path(''); // TODO: redirect to login page (if exists)
        }
        
        var message;
        var details;
        if(typeof response.data === 'object'){
            if(response.data.error == 404) {
                NotificationService.addInfo(response.data.msg || 'Resource not found');
            } else {
                message = response.data.msg || response.statusText; // if empty response message, take statusText
                NotificationService.addError(message, response.status, true, response.data.html);
            }
            
            if(response.data.notifications !== undefined) NotificationService.updateNotifications(response.data.notifications); 
        }else{
            message = response.status + ' ' + response.statusText;
            details = response.data; // html content is excepted
            NotificationService.addError(message, response.status, true, details);
        }
        
        return true; // proceed with success or error hooks of promise
    });
    
    $rootScope.getCurrentDateTime = function (){
        return new Date();
    };
    
    // Add feature to $location.url() function to be able to prevent reloading page (set reload param to false)
    var original = $location.url;
    $location.url = function (url, reload) {
        if (reload === false) {
            var lastRoute = $route.current;
            var un = $rootScope.$on('$locationChangeSuccess', function () {
                $route.current = lastRoute;
                un();
            });
        }
        return original.apply($location, [url]);
    };
}]);

// Controller for extension app in navigation bar
angular.module('AmpersandApp')
.controller('ExecEngineController', ["$scope", "Restangular", "NotificationService", function ($scope, Restangular, NotificationService) {
    $scope.run = function (){
        Restangular.one('admin/execengine/run').get()
        .then(
            function(data){ // on success
                data = data.plain();
                NotificationService.updateNotifications(data);
            }
        );
    };
}]);

angular.module('AmpersandApp')
.controller('InstallerController', ["$scope", "Restangular", "NotificationService", "RoleService", function ($scope, Restangular, NotificationService, RoleService) {
    $scope.installing = false;
    $scope.installed = false;
    
    $scope.install = function(defPop){
        $scope.installing = true;
        $scope.installed = false;
        Restangular.one('admin/installer').get({defaultPop : defPop}).then(function(data) {
            data = data.plain();
            NotificationService.updateNotifications(data);
            
            // deactive all roles
            RoleService.deactivateAllRoles();
            
            $scope.installing = false;
            $scope.installed = true;
        }, function(){
            $scope.installing = false;
            $scope.installed = false;
        });
    };
}]);

angular.module('uiSwitch', [])

.directive('switch', function(){
  return {
    restrict: 'AE'
  , replace: true
  , transclude: true
  , template: function(element, attrs) {
      var html = '';
      html += '<a href=""';
      html +=   (attrs.ngModel && !attrs.ngClick) ? ' ng-click="' + attrs.ngModel + '=!' + attrs.ngModel + '"' : '';
      html += '>';
      html += '<span';
      html +=   ' class="switch' + (attrs.class ? ' ' + attrs.class : '') + '"';
      html +=   ' ng-class="{ checked:' + attrs.ngModel + ' }"';
      html +=   '>';
      html +=   '<small></small>';
      html += '</span>';
      html += '<span ng-transclude></span>';
      html += '</a>';
      return html;
    }
  }
});
var app = angular.module('AmpersandApp');
app.requires[app.requires.length] = 'angularFileUpload'; // add angularFileUpload to dependency list
app.config(["$routeProvider", function($routeProvider) {
    $routeProvider
        .when('/ext/importer', {
            controller : 'PopulationImportController',
            templateUrl : 'app/src/importer/importer.html',
            interfaceLabel : 'Population importer'
        });
}]).service('ImportService', ["FileUploader", "NotificationService", function(FileUploader, NotificationService){
    let uploader = new FileUploader({
        url: 'api/v1/admin/import'
    });

    uploader.onSuccessItem = function(fileItem, response, status, headers) {
        NotificationService.updateNotifications(response.notifications);
    };
    
    uploader.onErrorItem = function(item, response, status, headers){
        let message;
        let details;
        if(typeof response === 'object'){
            message = response.msg || 'Error while importing';
            NotificationService.addError(message, status, true);
            
            if(response.notifications !== undefined) NotificationService.updateNotifications(response.notifications); 
        }else{
            message = status + ' Error while importing';
            details = response; // html content is excepted
            NotificationService.addError(message, status, true, details);
        }
    };
    
    return {uploader : uploader};
}]).controller('PopulationImportController', ["$scope", "ImportService", function ($scope, ImportService) {
    $scope.uploader = ImportService.uploader;
}]);

angular.module('AmpersandApp')
.controller('AtomicController', ["$scope", "ResourceService", function($scope, ResourceService){
    
    /*
     * Object to temporary store value/resourceId to add to list
     * Value/resourceId is stored as property of 'selected' obj. This is needed to pass it around by reference
     */
    $scope.selected = {};
    
    $scope.saveItem = ResourceService.saveItem; // function(resource, ifc, patchResource)
    
    $scope.addItem = ResourceService.addItem; // function(resource, ifc, selected, patchResource)
    
    $scope.removeItem = ResourceService.removeItem; // function(resource, ifc, index, patchResource)
    
    $scope.remove = ResourceService.removeResource; // function(parent, ifc, resource, patchResource)
    
    $scope.delete = ResourceService.deleteResource; // function(parent, ifc, resource)
}]);

angular.module('AmpersandApp')
.controller('AtomicDateController', ["$scope", "ResourceService", function ($scope, ResourceService) {
    
    $scope.isOpen = false;
    
    $scope.openDatepicker = function($event){
        $event.preventDefault();
        $event.stopPropagation();
        $scope.isOpen = true;
    };
    
    // Adds leading 0 if necesarry. Returns 2 digits.
    function pad(number) {
        var r = String(number);
        if ( r.length === 1 ) {
            r = '0' + r;
        }
        return r;
    }
    
    function modifyToJSON(obj){
        if(obj !== null){
            obj.toJSON = function(){
                return this.getUTCFullYear() + 
                    '-' + pad(this.getMonth() + 1) + // The getMonth() method returns the month in the specified date according to local time, as a zero-based value (where zero indicates the first month of the year).
                    '-' + pad(this.getDate());
            };
        }
    }
    
    $scope.selected = { value : ''}; // an empty object for temporary storing the input values
    
    $scope.saveDateItem = function(obj, property, patchResource){
        modifyToJSON(obj[property]);
        ResourceService.saveItem(obj, property, patchResource);
    };
    
    $scope.addDateItem = function(obj, property, selected, patchResource){
        if(selected.value !== ''){
            modifyToJSON(selected.value);
            ResourceService.addItem(obj, property, selected, patchResource);
        }else{
            console.log('Empty date selected');
        }
    };
}]);

angular.module('AmpersandApp')
.controller('AtomicTypeAheadController', ["$scope", "Restangular", "ResourceService", function($scope, Restangular, ResourceService){
    
    /*
     * Object to temporary store value/resourceId to add to list
     * Value/resourceId is stored as property of 'selected' obj. This is needed to pass it around by reference
     */
    $scope.selected = {};
    
    $scope.hasNoResults = false;
    
    /*
     * Typeahead object is declared in interface.controller.js
     * Thereby typeahead is called only once for every resourceType per interface
     */
    // $scope.typeahead = {};
    
    /*
     * Typeahead functionality
     * $scope.typeahead is initiated in InterfaceController to be able to reuse typeahead data
     */
    $scope.getTypeahead = function(resourceType, forceGetCall){
        forceGetCall = typeof forceGetCall !== 'undefined' ? forceGetCall : false;

        // Only if not yet set
        if(typeof $scope.typeahead[resourceType] === 'undefined' || forceGetCall){
            $scope.typeahead[resourceType] = Restangular.all('resource/' + resourceType).getList().$object;
        }
    };
    
    $scope.typeaheadOnSelect = function ($item, $model, $label, resource, ifc, patchResource){
        if(typeof $item._id_ === 'undefined') console.log('Resource id undefined');
        else if($item._id_ === '') console.log('Empty resource id provided');
        else{
            if(Array.isArray(resource[ifc])){
                // Construct patch(es)
                patch = ResourceService.createPatch('add', resource, patchResource, ifc, $item._id_);
                ResourceService.addPatches(patchResource, [patch])
                .then(function(data){
                    // Adapt in js model
                    if(!data.saved) resource[ifc].push(angular.copy($item));
                });
                
            }else if(resource[ifc] === null){
                // Construct patch(es)
                patch = ResourceService.createPatch('replace', resource, patchResource, ifc, $item._id_);
                ResourceService.addPatches(patchResource, [patch])
                .then(function(data){
                    // Adapt js model
                    if(!data.saved) resource[ifc] = angular.copy($item);
                });
            }
            else console.log('Error: Property already set and/or not defined');
            
            $scope.hasNoResults = false;
        }
        // Empty selected input
        $scope.selected.value = '';
    };
    
    $scope.typeAheadCreate = function (resource, ifc, selected, patchResource, resourceType){
        if(Array.isArray(resource[ifc])) { 
            ResourceService.addItem(resource, ifc, selected, patchResource).then(
                function(){
                    $scope.getTypeahead(resourceType, true);
                }
            );
        } else if(resource[ifc] === null) {
            resource[ifc] = selected.value;
            ResourceService.saveItem(resource, ifc, patchResource);
        } else {
            console.log('Error: Property already set and/or not defined');
        }
    };
}]);

angular.module('AmpersandApp')
.controller('AtomicUploadFileController', ["$scope", "FileUploader", "NotificationService", function($scope, FileUploader, NotificationService){
    
    // File uploader stuff
    $scope.FileUploader = new FileUploader({
        alias : 'file', // fieldname as used in $_FILES['file']
        formData : [],
        removeAfterUpload : true,
        autoUpload : true
    });
    
    $scope.FileUploader.onSuccessItem = function(fileItem, response, status, headers){
        NotificationService.updateNotifications(response.notifications);
        
        newResource = response.content;
        
        // Add new resource to ifc
        if(Array.isArray(fileItem.resource[fileItem.ifc])){ // non-uni = list
            fileItem.resource[fileItem.ifc].splice(-1, 0, newResource);
        }else{ // uni = object
            fileItem.resource[fileItem.ifc] = newResource;
        }
    };
    
    $scope.FileUploader.onErrorItem = function(item, response, status, headers){
        NotificationService.addError(response.msg, response.error, true, response.html);
        NotificationService.updateNotifications(response.notifications);
    };
}]);

angular.module('AmpersandApp')
.controller('BoxController', ["$scope", "ResourceService", function($scope, ResourceService){
    
    // Function to create a new resource (does a POST)
    $scope.createResource = ResourceService.createResource; // function(resource, ifc, callingObj, insertAtIndex)
    
    // Function to save certain attributes changes of a resource (does a PATCH)
    $scope.save = ResourceService.saveResource; // function(resource)
    
    // Function to cancel unsaved edits (does a GET)
    $scope.cancel = ResourceService.cancelResource; // function(resource)
    
    // Function to remove a resource from an interface (list)
    $scope.remove = ResourceService.removeResource; // function(ifc, resource, patchResource)
    
    // Function to delete a resource
    $scope.delete = ResourceService.deleteResource; // function(ifc, resource)
}]);

angular.module('AmpersandApp')
.controller('InterfaceController', ["$scope", "$location", "ResourceService", function($scope, $location, ResourceService){
    /*
     * An empty object for typeahead functionality.
     * Defined here so it can be reused in an interface
     * Prevents multiple calls for the same resourceType
     */
    $scope.typeahead = {};
    
    // Detects location changes and checks if there are unsaved changes
    $scope.$on("$locationChangeStart", function(event, next, current){
        if(ResourceService.checkRequired()){
            confirmed = confirm("You have unsaved edits. Do you wish to leave?");
            if (event && !confirmed) event.preventDefault();
            else if(event && confirmed) ResourceService.emptyUpdatedResources();
            else console.log('Someting went wrong. Cannot determine action after locationChangeStart');
        }
    });
    
    // Function to change location to create a new resource
    $scope.newResource = function(){
        $location.url('/' + ifcName + '?new');
    };
    
    // Function (reference) to check if there are pending promises for a resource
    $scope.pendingPromises = ResourceService.pendingPromises;
}]);

angular.module('AmpersandApp')
.directive('myBluronenter', function() {
    return function(scope, element, attrs) {
        element.bind("keydown keypress", function(event) {
            if(event.which === 13) { // 13 = Carriage return
                event.target.blur();

                event.preventDefault();
            }
        });
    };
});

angular.module('AmpersandApp')
.directive('myShowonhoverBox', function (){
    return {
        link : function(scope, element, attrs) {
            if(!element.closest('.box').hasClass('my-showonhover-box-show')) element.hide(); // default hide
            
            element.closest('.box').bind('mouseenter', function() {
                element.closest('.box').addClass('my-showonhover-box-show');
                element.show();
            });
            element.closest('.box').bind('mouseleave', function() {
                element.closest('.box').removeClass('my-showonhover-box-show');
                element.hide();
            });
        }
    };
});

angular.module('AmpersandApp')
.service('ResourceService', ["$localStorage", "$timeout", "$location", "Restangular", "NotificationService", "$q", function($localStorage, $timeout, $location, Restangular, NotificationService, $q){
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
            if(!Array.isArray(callingObj._loading_)) callingObj._loading_ = [];
            callingObj._loading_.push(promise);
            
            return promise;
        },

        /**
         * Patch the given resource by calling the API and sending the list of stored patches 
         * 
         * @param {Object} resource
         * @returns {Promise}
         */
        saveResource : function(resource){
            promise = Restangular
            .one(resource._path_)
            .patch(resource._patchesCache_, {})
            .then(function(data) {
                data = data.plain();
                
                // Update resource data if committed
                if(data.isCommitted) {
                    if(resource._isRoot_) resource.get();
                    else resource = angular.extend(resource, data.content);
                }
                
                // Update visual feedback (notifications and buttons)
                ResourceService.processResponse(resource, data);

                return {resource : resource, saved: true};
            });

            // Add promise to loading list
            if(!Array.isArray(resource._loading_)) resource._loading_ = [];
            resource._loading_.push(promise);
            
            return promise;
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
            if(!Array.isArray(resource._loading_)) resource._loading_ = [];
            resource._loading_.push(promise);
            
            return promise;
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
            if(!Array.isArray(callingObj._loading_)) callingObj._loading_ = [];
            callingObj._loading_.push(promise);
            
            return promise;
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
            patch = ResourceService.createPatch('remove', resource, patchResource);

            // Execute patch
            return ResourceService
            .addPatches(patchResource, [patch])
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
                if(!Array.isArray(resource._loading_)) resource._loading_ = [];
                resource._loading_.push(promise);
                
                return promise;
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
            patch = ResourceService.createPatch('replace', resource, patchResource, ifc, value);

            // Register patch
            return ResourceService.addPatches(patchResource, [patch]);
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
                patch = ResourceService.createPatch('add', resource, patchResource, ifc, selected.value);
                return ResourceService.addPatches(patchResource, [patch]).then(function(data){
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
            patch = ResourceService.createPatch('remove', resource, patchResource, ifc, value);
            
            // Adapt js model
            resource[ifc].splice(index, 1);

            return ResourceService.addPatches(patchResource, [patch]);
        },
        
        /**
         * Construct patch object (with attributes 'op', 'path' and 'value')
         * 
         * @param {string} operation choose from 'add', 'remove' or 'replace'
         * @param {Object} resource
         * @param {Object} patchResource
         * @param {string} ifc
         * @param {string} value
         * @returns {Object}
         */
        createPatch : function(operation, resource, patchResource, ifc, value){
            if(typeof patchResource === 'undefined') patchResource = resource;
            pathLength = patchResource._path_.length;
            
            path = resource._path_.substring(pathLength);
            if(typeof ifc !== 'undefined') path = path + '/' + ifc;
            
            if(typeof value === 'undefined') return { op : operation, path : path};
            else return { op : operation, path : path, value : value};
        },
        
        /**
         * Add list of patches for given resource and call API (when auto-save is on)
         * 
         * @param {Object} resource
         * @param {Object[]} patches
         * @returns {Promise}
         */
        addPatches : function(resource, patches){
            // Add new patches to resource
            if(!Array.isArray(resource._patchesCache_)) resource._patchesCache_ = [];
            resource._patchesCache_ = resource._patchesCache_.concat(patches);
            
            // Add resource to updatedResources
            if(updatedResources.indexOf(resource) === -1) updatedResources.push(resource);
            
            // Save if autoSave is enabled
            if($localStorage.autoSave) {
                return ResourceService.saveResource(resource);
            } else {
                // Update visual feedback
                ResourceService.setResourceStatus(resource, 'warning');
                resource._showButtons_ = {'save' : true, 'cancel' : true};
                return $q.resolve({resource : resource, saved : false});
            }
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
        }
    };
    
    return ResourceService;
}]);

angular.module('AmpersandApp')
.directive('myNavbarResize', ["$window", "$timeout", "NavigationBarService", function ($window, $timeout, NavigationBarService) {
    return function (scope, element) {
        var w = angular.element($window);
        
        var resizeNavbar = function() {
            $timeout(function(){
                // moving ifc items from dropdown-menu to navbar itself
                while($('#navbar-interfaces').width() < ($('#navbar-wrapper').width() - $('#navbar-options').width()) &&
                        $('#navbar-interfaces-dropdown-menu').children().length > 0){
                    $("#navbar-interfaces-dropdown-menu").children().first().appendTo("#navbar-interfaces");
                }
                
                // moving ifc items from navbar to dropdown-menu
                while($('#navbar-interfaces').width() > ($('#navbar-wrapper').width() - $('#navbar-options').width())){
                    $("#navbar-interfaces").children().last().prependTo("#navbar-interfaces-dropdown-menu");
                    
                    // show/hide dropdown menu for more interfaces (must be inside loop, because it affects the width of the navbar
                    $('#navbar-interfaces-dropdown').toggleClass('hidden', $('#navbar-interfaces-dropdown-menu').children().length <= 0);
                }
                
                // show/hide dropdown menu when possible
                $('#navbar-interfaces-dropdown').toggleClass('hidden', $('#navbar-interfaces-dropdown-menu').children().length <= 0);
            });
        };
        
        // watch navbar
        scope.$watch('NavigationBarService.navbar', function() {
            // small timeout (500ms) for angular to update DOM after navbar data change
            // TODO: instead watch resize of element '#navbar-interfaces' and '#navbar-options'
            $timeout(function(){
                resizeNavbar();
            }, 500);
        });
        
        // when window size gets changed
        w.bind('resize', function () {
            resizeNavbar();
        });
        
        // when page loads
        angular.element(document).ready(function(){
            resizeNavbar();
        });
    };
}]);

angular.module('AmpersandApp')
.controller('NavigationBarController', ["$scope", "$route", "Restangular", "$localStorage", "$sessionStorage", "$location", "NotificationService", "RoleService", "NavigationBarService", function ($scope, $route, Restangular, $localStorage, $sessionStorage, $location, NotificationService, RoleService, NavigationBarService) {
    
    $scope.localStorage = $localStorage;
    $scope.sessionStorage = $sessionStorage;
    $scope.loadingNavBar = [];
    $scope.navbar = NavigationBarService.navbar;
    $scope.resetSettingsToDefault = NavigationBarService.resetSettingsToDefault;
    
    $scope.reload = function(){
        $scope.loadingNavBar = [];
        $scope.loadingNavBar.push(NavigationBarService.refreshNavBar());
        $route.reload();
    };

    $scope.toggleRole = function(roleId, set){
        RoleService.toggleRole(roleId, set);
        $scope.loadingNavBar = [];
        $scope.loadingNavBar.push(
            RoleService.setActiveRoles()
            .then(function(data){
                NavigationBarService.refreshNavBar();
            })
        );
    };

    $scope.checkAllRules = NotificationService.checkAllRules;

    $scope.createNewResource = function(resourceType, openWithIfc){
        Restangular.one('resource').all(resourceType)
        .post({}, {})
        .then(
            function(data){
                // Jumps to interface and requests newly created resource
                $location.url(openWithIfc + '/' + data._id_);
            }
        );
    };
    
    $scope.loadingNavBar.push(NavigationBarService.refreshNavBar());
}]);

angular.module('AmpersandApp')
.service('NavigationBarService', ["Restangular", "$localStorage", "$sessionStorage", "$timeout", "NotificationService", function(Restangular, $localStorage, $sessionStorage, $timeout, NotificationService){
    let navbar = {
        top: [],
        new: [],
        refresh: [],
        role: [],
        ext: []
    };
    let defaultSettings = {
        notify_showSignals: true,
        notify_showInfos: true,
        notify_showSuccesses: true,
        notify_autoHideSuccesses: true,
        notify_showErrors: true,
        notify_showWarnings: true,
        notify_showInvariants: true,
        autoSave: true
    };

    let service = {
        navbar : navbar,
        defaultSettings : defaultSettings,

        refreshNavBar : function(){
            return Restangular
            .one('app/navbar')
            .get()
            .then(function(data){
                data = data.plain();

                // Content of navbar
                navbar.top = data.top;
                navbar.new = data.new;
                navbar.refresh = data.refresh;
                navbar.role = data.role;
                navbar.ext = data.ext;

                // Content for session storage
                $sessionStorage.session = data.session;
                $sessionStorage.sessionRoles = data.sessionRoles;
                $sessionStorage.sessionVars = data.sessionVars;
                
                // Save default settings
                service.defaultSettings = data.defaultSettings;
                service.initializeSettings();
                
                // Update notifications
                NotificationService.updateNotifications(data.notifications);
            }, function(error){
                service.initializeSettings();
            });
        },

        initializeSettings : function(){
            let resetRequired = false;

            // Check for undefined settings
            angular.forEach(service.defaultSettings, function(value, index, obj){
                if($localStorage[index] === undefined) {
                    resetRequired = true;
                }
            });

            if(resetRequired) service.resetSettingsToDefault();
        },

        resetSettingsToDefault : function(){
            // all off
            angular.forEach(service.defaultSettings, function(value, index, obj){
                $localStorage[index] = false;
            });
            
            $timeout(function() {
                // Reset to default
                $localStorage.$reset(service.defaultSettings);
            }, 500);
        }
    };
    
    return service;
}]);

angular.module('AmpersandApp')
.service('RoleService', ["$sessionStorage", "Restangular", function($sessionStorage, Restangular){
    
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
        },
        
        setActiveRoles : function(){
            return Restangular.all('app/roles').patch($sessionStorage.sessionRoles);
        }
    };
    
    return RoleService;
}]);

angular.module('AmpersandApp')
.service('NotificationService', ["$localStorage", "$sessionStorage", "$timeout", "Restangular", function($localStorage, $sessionStorage, $timeout, Restangular){
    // Initialize notifications container
    let notifications = {
        'signals' : [],
        'invariants' : [],
        'infos' : [],
        'successes' : [],
        'warnings' : [],
        'errors' : []
    };
    
    let NotificationService = {
        notifications : notifications,
        
        // Function to get notifications again
        getNotifications : function(){
            return Restangular
            .one('app/notifications')
            .get()
            .then(
                function(data){
                    data = data.plain();
                    NotificationService.updateNotifications(data);
                },
                function(){
                    NotificationService.addError('Something went wrong while getting notifications');
                }
            );
        },

        checkAllRules : function(){
            return Restangular
            .one('admin/ruleengine/evaluate/all')
            .get()
            .then(
                function(data){
                    data = data.plain();
                    NotificationService.addSuccess('Evaluated all rules.');
                    NotificationService.updateNotifications(data);
                },function(){
                    NotificationService.addError('Something went wrong while evaluating all rules');
                }
            );
        },
        
        // Function to update notifications after api response
        updateNotifications : function(data){
            if(data === undefined) return;
            
            // Overwrite
            notifications.signals = data.signals;
            notifications.invariants = data.invariants;
            
            // Merge
            notifications.infos = notifications.infos.concat(data.infos);
            notifications.successes = notifications.successes.concat(data.successes);
            notifications.warnings = notifications.warnings.concat(data.warnings);
            notifications.errors = notifications.errors.concat(data.errors);
            
            if($localStorage.notify_autoHideSuccesses){
                $timeout(function() {
                    notifications.successes = [];
                }, 3000);
            }
        },
        
        addSuccess : function(message){
            notifications.successes.push({
                'message' : message,
                'count' : 1
            });
            
            // TODO: move timeout function here for auto hide successes
        },
        
        addError : function(message, code, persistent, details){
            code = typeof code !== undefined ? code : null;
            persistent = typeof persistent !== undefined ? persistent : false;
            details = typeof details !== undefined ? details : false;
            
            let alreadyExists = false;
            let arr = notifications.errors;
            for (let i = 0; i < arr.length; i++) {
                if (arr[i].message == message) {
                    arr[i].count += 1;
                    arr[i].code = code;
                    arr[i].persistent = persistent;
                    arr[i].details = details;
                    alreadyExists = true;
                }
            }
            if(!alreadyExists) notifications.errors.push({
                'message' : message,
                'code' : code,
                'count' : 1,
                'persistent' : persistent,
                'details' : details
            });
        },
        
        addWarning : function(message){
            let alreadyExists = false;
            let arr = notifications.warnings;
            for (var i = 0; i < arr.length; i++) {
                if (arr[i].message == message) {
                    arr[i].count += 1;
                    alreadyExists = true;
                }
            }
            if(!alreadyExists) notifications.warnings.push({
                'message' : message,
                'count' : 1
            });
        },
        
        addInfo : function(message){
            let alreadyExists = false;
            let arr = notifications.infos;
            for (var i = 0; i < arr.length; i++) {
                if (arr[i].message == message) {
                    arr[i].count += 1;
                    alreadyExists = true;
                }
            }
            if(!alreadyExists) notifications.infos.push({
                'message' : message,
                'count' : 1
            });
        }
    };
    
    return NotificationService;
}]);
angular.module('AmpersandApp')
.controller('NotificationCenterController', ["$scope", "$route", "Restangular", "$localStorage", "NotificationService", function ($scope, $route, Restangular, $localStorage, NotificationService) {
    
    $scope.localStorage = $localStorage;
    $scope.notifications = NotificationService.notifications;
    
    // Hide success-, error-, warnings-, info- and invariant violation messages (not signals) upon route change
    $scope.$on("$routeChangeSuccess", function(){
        $scope.notifications.successes = [];
        $scope.notifications.errors = $scope.notifications.errors.filter(function (error){
            if(error.persistent){
                error.persistent = false;
                return true;
            }
            else return false;
        });
        $scope.notifications.warnings = [];
        $scope.notifications.infos = [];
        $scope.notifications.invariants = [];
    });
    
    // Function to close notifications
    $scope.closeAlert = function(alerts, index) {
        alerts.splice(index, 1);
    };
    
}]);

angular.module('AmpersandApp')
.filter('unsafe', ["$sce", function($sce){
    return $sce.trustAsHtml;
}]);

angular.module('AmpersandApp')
.value('cgBusyDefaults',{
    message:'Loading...',
    backdrop: true,
    //templateUrl: 'my_custom_template.html',
    //delay: 500, // in ms
    minDuration: 500, // in ms
    // wrapperClass: 'my-class my-class2'
});

angular.module('AmpersandApp')
.directive('myNavToInterfaces', function(){
    return {
        restrict : 'E',
        scope : {resource : '=', target : '@'}, // '=' => two-way bind, '@' => evaluates string (use {{}} in html) 
        templateUrl : 'app/src/shared/myNavTo/myNavToInterfaces.html',
        transclude : true
    };
});

angular.module('AmpersandApp')
.directive('myNavToOtherInterfaces', function(){
    return {
        restrict : 'E',
        scope  : {resource : '=', target : '@'}, // '=' => two-way bind, '@' => evaluates string (use {{}} in html) 
        templateUrl : 'app/src/shared/myNavTo/myNavToOtherInterfaces.html'
    };
});

angular.module('AmpersandApp').run(['$templateCache', function($templateCache) {$templateCache.put('app/src/admin/check-rules-menu-item.html','<a ng-click="checkAllRules()"><span class="glyphicon glyphicon-check"></span><span> (Re)evaluate all rules</span></a>');
$templateCache.put('app/src/admin/execengine-menu-item.html','<a ng-controller="ExecEngineController" href="" ng-click="run()">\r\n\t<span class="glyphicon glyphicon-cog"></span><span> Run execution engine</span>\r\n</a>');
$templateCache.put('app/src/admin/exporter-menu-item.html','<a ng-href="api/v1/admin/export/all">\r\n    <span class="glyphicon glyphicon-download"></span><span> Population export</span>\r\n</a>');
$templateCache.put('app/src/admin/installer-menu-item.html','<a href="#/admin/installer">\r\n    <span class="glyphicon glyphicon-trash"></span><span> Reinstall database</span>\r\n</a>');
$templateCache.put('app/src/admin/installer.html','<div class="container-fluid" id="Interface">\r\n    <div class="jumbotron">\r\n        <h1>Installer</h1>\r\n        <p>This action will reinstall the application and delete all content.</p>\r\n        <p>If provided, the initial population will be installed.</p>\r\n        <div class="btn-group">\r\n            <button type="button" ng-click="install(true)" class="btn btn-lg" ng-class="{\'btn-danger\' : (!installing && !installed), \'btn-warning\' : installing, \'btn-success\' : installed}" ng-disabled="installing">\r\n                <span ng-if="!installed && ! installing">Reinstall application  </span>\r\n                <span ng-if="installing">Application installing  </span>\r\n                <span ng-if="installed">Application reinstalled  </span>\r\n                <img ng-if="installing" ng-src="app/images/loading.gif" style="height:20px;"/>\r\n            </button>\r\n            <button type="button" class="btn btn-lg dropdown-toggle" ng-class="{\'btn-danger\' : (!installing && !installed), \'btn-warning\' : installing, \'btn-success\' : installed}" ng-disabled="installing" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">\r\n                <span class="caret"></span>\r\n            </button>\r\n            <ul class="dropdown-menu">\r\n                <li><a href="" ng-click="install(true)">Reinstall application</a></li>\r\n                <li><a href="" ng-click="install(false)">Reinstall application (without default population)</a></li>\r\n            </ul>\r\n        </div>\r\n    </div>\r\n</div>');
$templateCache.put('app/src/importer/importer.html','<style>\r\n.my-drop-zone { \r\nborder: dotted 3px lightgray;\r\n}\r\n\r\n/* Default class applied to drop zones on over */\r\n.nv-file-over {\r\n\tborder: dotted 3px red;\r\n}\r\n\r\n.another-file-over-class {\r\n\tborder: dotted 3px green;\r\n}\r\n</style>\r\n<div class="container-fluid interface">\r\n\t<fieldset>\r\n\t\t<legend>Population importer</legend>\r\n\t\t<div class="row">\r\n\t\t\t<div class="col-md-3" nv-file-drop="" uploader="uploader">\r\n\t\t\t\t<h3>Select files</h3>\r\n\t\t\t\t\r\n\t\t\t\t<div ng-show="uploader.isHTML5">\r\n\t\t\t\t<!-- 3. nv-file-over uploader="link" over-class="className" -->\r\n\t\t\t\t\t<div class="well my-drop-zone" nv-file-over="" uploader="uploader">\r\n\t\t\t\t\t\tBase drop zone\r\n\t\t\t\t\t</div>\r\n\t\t\t\t</div>\r\n\t\t\t\t\r\n\t\t\t\t<!-- Example: nv-file-select="" uploader="{Object}" options="{Object}" filters="{String}" -->\r\n\t\t\t\tMultiple\r\n\t\t\t\t<input type="file" nv-file-select="" uploader="uploader" multiple  /><br/>\r\n\t\t\t\t\r\n\t\t\t\tSingle\r\n\t\t\t\t<input type="file" nv-file-select="" uploader="uploader" />\r\n\t\t\t</div>\t\r\n\t\t\t\r\n\t\t\t<div class="col-md-9" style="margin-bottom: 40px">\r\n\t\r\n\t\t\t\t<h3>Upload queue</h3>\r\n\t\t\t\t<p>Queue length: {{ uploader.queue.length }}</p>\r\n\t\t\t\t\r\n\t\t\t\t<table class="table">\r\n\t\t\t\t\t<thead>\r\n\t\t\t\t\t\t<tr>\r\n\t\t\t\t\t\t\t<th width="50%">Name</th>\r\n\t\t\t\t\t\t\t<th ng-show="uploader.isHTML5">Size</th>\r\n\t\t\t\t\t\t\t<th ng-show="uploader.isHTML5">Progress</th>\r\n\t\t\t\t\t\t\t<th>Status</th>\r\n\t\t\t\t\t\t\t<th>Actions</th>\r\n\t\t\t\t\t\t</tr>\r\n\t\t\t\t\t</thead>\r\n\t\t\t\t\t<tbody>\r\n\t\t\t\t\t\t<tr ng-repeat="item in uploader.queue">\r\n\t\t\t\t\t\t\t<td><strong>{{ item.file.name }}</strong></td>\r\n\t\t\t\t\t\t\t<td ng-show="uploader.isHTML5" nowrap>{{ item.file.size/1024/1024|number:2 }} MB</td>\r\n\t\t\t\t\t\t\t<td ng-show="uploader.isHTML5">\r\n\t\t\t\t\t\t\t\t<div class="progress" style="margin-bottom: 0;">\r\n\t\t\t\t\t\t\t\t\t<div class="progress-bar" role="progressbar" ng-style="{ \'width\': item.progress + \'%\' }"></div>\r\n\t\t\t\t\t\t\t\t</div>\r\n\t\t\t\t\t\t\t</td>\r\n\t\t\t\t\t\t\t<td class="text-center">\r\n\t\t\t\t\t\t\t\t<span ng-show="item.isSuccess"><i class="glyphicon glyphicon-ok"></i></span>\r\n\t\t\t\t\t\t\t\t<span ng-show="item.isCancel"><i class="glyphicon glyphicon-ban-circle"></i></span>\r\n\t\t\t\t\t\t\t\t<span ng-show="item.isError"><i class="glyphicon glyphicon-remove"></i></span>\r\n\t\t\t\t\t\t\t\t<span ng-show="item.isUploading"><img src="app/images/loading.gif" height="20" width="20"></span>\r\n\t\t\t\t\t\t\t</td>\r\n\t\t\t\t\t\t\t<td nowrap>\r\n\t\t\t\t\t\t\t\t<button type="button" class="btn btn-success btn-xs" ng-click="item.upload()" ng-disabled="item.isReady || item.isUploading">  <!-- Removed: "|| item.isSuccess" to enable the re-upload of a file.-->\r\n\t\t\t\t\t\t\t\t\t<span class="glyphicon glyphicon-upload"></span> Upload\r\n\t\t\t\t\t\t\t\t</button>\r\n\t\t\t\t\t\t\t\t<button type="button" class="btn btn-warning btn-xs" ng-click="item.cancel()" ng-disabled="!item.isUploading">\r\n\t\t\t\t\t\t\t\t\t<span class="glyphicon glyphicon-ban-circle"></span> Cancel\r\n\t\t\t\t\t\t\t\t</button>\r\n\t\t\t\t\t\t\t\t<button type="button" class="btn btn-danger btn-xs" ng-click="item.remove()">\r\n\t\t\t\t\t\t\t\t\t<span class="glyphicon glyphicon-trash"></span> Remove\r\n\t\t\t\t\t\t\t\t</button>\r\n\t\t\t\t\t\t\t</td>\r\n\t\t\t\t\t\t</tr>\r\n\t\t\t\t\t</tbody>\r\n\t\t\t\t</table>\r\n\t\t\t\t\r\n\t\t\t\t<div>\r\n\t\t\t\t\t<div>\r\n\t\t\t\t\t\tQueue progress:\r\n\t\t\t\t\t\t<div class="progress">\r\n\t\t\t\t\t\t\t<div class="progress-bar" role="progressbar" ng-style="{ \'width\': uploader.progress + \'%\' }"></div>\r\n\t\t\t\t\t\t</div>\r\n\t\t\t\t\t</div>\r\n\t\t\t\t\t<button type="button" class="btn btn-success btn-sm" ng-click="uploader.uploadAll()" ng-disabled="!uploader.getNotUploadedItems().length">\r\n\t\t\t\t\t\t<span class="glyphicon glyphicon-upload"></span> Upload all\r\n\t\t\t\t\t</button>\r\n\t\t\t\t\t<button type="button" class="btn btn-warning btn-sm" ng-click="uploader.cancelAll()" ng-disabled="!uploader.isUploading">\r\n\t\t\t\t\t\t<span class="glyphicon glyphicon-ban-circle"></span> Cancel all\r\n\t\t\t\t\t</button>\r\n\t\t\t\t\t<button type="button" class="btn btn-danger btn-sm" ng-click="uploader.clearQueue()" ng-disabled="!uploader.queue.length">\r\n\t\t\t\t\t\t<span class="glyphicon glyphicon-trash"></span> Remove all\r\n\t\t\t\t\t</button>\r\n\t\t\t\t</div>\t\r\n\t\t\t</div>\r\n\t\t</div>\r\n\t</fieldset>\r\n</div>');
$templateCache.put('app/src/importer/menu-item.html','<a ng-href="#/ext/importer/">\r\n    <span class="glyphicon glyphicon-upload"></span><span> Population importer</span>\r\n</a>');
$templateCache.put('app/src/navbar/navigationBar.html','<nav class="navbar navbar-default" role="navigation" ng-controller="NavigationBarController" cg-busy="{promise:loadingNavBar}">\r\n    <div id="navbar-wrapper" class="container">\r\n        <ul class="nav navbar-nav" id="navbar-interfaces" my-navbar-resize>\r\n            <li><a href="#/"><span class="glyphicon glyphicon-home"></span></a></li>\r\n            <li id="navbar-interfaces-dropdown" class="dropdown" uib-tooltip="More interfaces" tooltip-trigger="mouseenter" tooltip-placement="top">\r\n                <a href="" class="dropdown-toggle" data-toggle="dropdown"><span class="glyphicon glyphicon-menu-hamburger"></span></a>\r\n                <ul id="navbar-interfaces-dropdown-menu" class="dropdown-menu" role="menu"></ul>\r\n            </li>\r\n            <li id="{{interface.label}}" ng-repeat="interface in navbar.top"> <!-- the interface id is there so we can style specific menu items with css -->\r\n                <a href="#/{{interface.id}}">\r\n                    <span class="glyphicon glyphicon-list-alt"></span> {{interface.label}}</a>\r\n            </li>\r\n        </ul>\r\n        <ul class="nav navbar-nav navbar-right" id="navbar-options">\r\n            <!-- hidden on extra small devices, e.g. phone (<768px) -->\r\n            <li class="dropdown hidden-xs" uib-tooltip="Show/hide notifications" tooltip-trigger="mouseenter" tooltip-placement="left">\r\n                <a href="" class="dropdown-toggle" data-toggle="dropdown"><span class="glyphicon glyphicon-bullhorn"></span></a>\r\n                <ul class="dropdown-menu" role="menu" ng-click="$event.stopPropagation();">\r\n                    <li class="dropdown-header">Transaction settings</li>\r\n                        <li><switch ng-model="localStorage.notify_showSignals"> Show signals</switch></li>\r\n                        <li><switch ng-model="localStorage.notify_showInvariants"> Show invariants</switch></li>\r\n                        <li><switch ng-model="localStorage.autoSave"> Auto save changes</switch></li>\r\n                    <li class="dropdown-header">User logs</li>\r\n                        <li><switch ng-model="localStorage.notify_showErrors"> Show errors</switch></li>\r\n                        <li><switch ng-model="localStorage.notify_showWarnings"> Show warnings</switch></li>\r\n                        <li><switch ng-model="localStorage.notify_showInfos"> Show infos</switch></li>\r\n                        <li><switch ng-model="localStorage.notify_showSuccesses"> Show successes</switch></li>\r\n                        <li><switch ng-model="localStorage.notify_autoHideSuccesses"> Auto hide successes</switch></li>\r\n                    <li class="divider" role="presentation"></li>\r\n                        <li><a href="" ng-click="resetSettingsToDefault();"><span class="glyphicon glyphicon-repeat" style="margin: 4px; width: 30px;"></span> Default settings</a></li>\r\n                </ul>\r\n            </li>\r\n            \r\n            <!-- hidden on extra small devices, e.g. phone (<768px) -->\r\n            <li class="dropdown hidden-xs" uib-tooltip="Refresh/reset options" tooltip-trigger="mouseenter" tooltip-placement="top">\r\n                <a href="" class="dropdown-toggle" data-toggle="dropdown"><span class="glyphicon glyphicon-refresh"></span></a>\r\n                <ul class="dropdown-menu" role="menu">\r\n                    <li><a href="" ng-click="reload()"><span class="glyphicon glyphicon-refresh"></span> Refresh page</a></li>\r\n                    <li ng-if="navbar.refresh.length" class="divider" role="presentation"></li>\r\n                    <li ng-repeat="app in navbar.refresh" ng-include="app.url"/>\r\n                </ul>\r\n            </li>\r\n            \r\n            <!-- hidden on extra small devices, e.g. phone (<768px) -->\r\n            <li ng-if="navbar.ext.length" class="dropdown hidden-xs" uib-tooltip="Select application extensions" tooltip-trigger="mouseenter" tooltip-placement="top">\r\n                <a href="" class="dropdown-toggle" data-toggle="dropdown"><span class="glyphicon glyphicon-th"></span></a>\r\n                <ul class="dropdown-menu" role="menu">\r\n                    <li ng-repeat="ext in navbar.ext" ng-include="ext.url"/>\r\n                    <!-- <li class="divider" role="presentation"></li>-->                    \r\n                </ul>\r\n            </li>\r\n            \r\n            <!-- hidden on extra small devices, e.g. phone (<768px) -->\r\n            <li ng-if="navbar.new.length" class="dropdown hidden-xs" uib-tooltip="Create new resource" tooltip-trigger="mouseenter" tooltip-placement="top">\r\n                <a href="" class="dropdown-toggle" data-toggle="dropdown">\r\n                    <span class="glyphicon glyphicon-plus"></span>\r\n                </a>\r\n                <ul class="dropdown-menu" role="menu">\r\n                    <li ng-repeat="item in navbar.new" ng-class="{\'dropdown-submenu\' : item.ifcs.length > 1}">\r\n                        <!--<a  style="position:relative; display:inline-block;">-->\r\n                        <a ng-if="item.ifcs.length > 1" tabindex="-1" href="#">{{item.label}}</a>\r\n                        <ul ng-if="item.ifcs.length > 1" class="dropdown-menu" role="menu">\r\n                            <li ng-repeat="ifc in item.ifcs">\r\n                                <a tabindex="-1" href="#" ng-click="createNewResource(ifc.resourceType, ifc.link);">{{ifc.label}}</a>\r\n                            </li>\r\n                        </ul>\r\n                        \r\n                        <a ng-if="item.ifcs.length == 1" href="" ng-click="createNewResource(item.ifcs[0].resourceType, item.ifcs[0].link);">{{item.label}}</a>\r\n                        <span ng-if="item.ifcs.length == 0">{{item.label}}</span>\r\n                    </li>\r\n                </ul>\r\n            </li>\r\n            \r\n            <li ng-if="sessionStorage.sessionRoles.length || navbar.role.length" class="dropdown" uib-tooltip="Switch roles" tooltip-trigger="mouseenter" tooltip-placement="top">\r\n                <a href="" class="dropdown-toggle" data-toggle="dropdown"><span class="glyphicon glyphicon-user"></span></a>\r\n                <ul class="dropdown-menu" role="menu">\r\n                    <li ng-repeat="role in sessionStorage.sessionRoles" ng-click="$event.stopPropagation();"><switch ng-model="role.active" ng-click="toggleRole(role.id);"> {{role.label}}</switch></li>\r\n                    <li ng-if="navbar.role.length && sessionStorage.sessionRoles.length" class="divider" role="presentation"></li>\r\n                    <li ng-repeat="ext in navbar.role" ng-include="ext.url"/>\r\n                </ul>\r\n            </li>\r\n        </ul>\r\n    </div>\r\n</nav>');
$templateCache.put('app/src/notifications/notificationCenter.html','<div class="container-fluid">\r\n    <div id="notificationCenter" ng-controller="NotificationCenterController">\r\n        \r\n        <div id="infos" ng-show="localStorage.notify_showInfos">\r\n            <div class="alert alert-info alert-dismissible" role="alert" ng-repeat="info in notifications.infos">\r\n                <button type="button" class="close" data-dismiss="alert" aria-label="Close" ng-click="closeAlert(notifications.infos, $index);"><span aria-hidden="true">&times;</span></button>\r\n                <span class="glyphicon glyphicon-info-sign"></span><span> {{info.message}}</span>\r\n            </div>\r\n        </div>\r\n        \r\n        <div id="warnings" ng-show="localStorage.notify_showWarnings">\r\n            <div class="alert alert-warning alert-dismissible" role="alert" ng-repeat="warning in notifications.warnings">\r\n                <button type="button" class="close" data-dismiss="alert" aria-label="Close" ng-click="closeAlert(notifications.warnings, $index);"><span aria-hidden="true">&times;</span></button>\r\n                <span class="glyphicon glyphicon-warning-sign"></span><span> {{warning.message}}</span>\r\n                <span class="badge pull-right" ng-show="warning.count > 1">{{warning.count}}</span>\r\n            </div>\r\n        </div>\r\n        \r\n        <div id="errors" ng-show="localStorage.notify_showErrors">\r\n            <div class="panel panel-danger" id="error-panel-{{key}}" ng-repeat="(key, error) in notifications.errors">\r\n                <div class="panel-heading btn btn-block" data-toggle="collapse" data-target="#error-body-{{key}}">\r\n                    <div class="text-left">\r\n                        <span class="glyphicon glyphicon-exclamation-sign"></span> <span ng-bind-html="error.message | unsafe"></span>\r\n                        <button type="button" class="close" data-target="#error-panel-{{key}}" data-dismiss="alert" aria-label="Dismiss" ng-click="closeAlert(notifications.errors, $index);">\r\n                            <span aria-hidden="true">&times;</span>\r\n                        </button>\r\n                        <span class="badge pull-right" ng-show="error.count > 1">{{error.count}}</span>\r\n                    </div>\r\n                </div>\r\n                <div class="panel-body collapse" id="error-body-{{key}}">\r\n                    <div ng-if="error.details" ng-bind-html="error.details | unsafe"></div>\r\n                </div>\r\n            </div>\r\n        </div>\r\n        \r\n        <div id="invariants" ng-show="localStorage.notify_showInvariants">\r\n            <div class="panel panel-danger" ng-repeat="(key, val) in notifications.invariants">\r\n                <div class="panel-heading btn btn-block" data-toggle="collapse" data-target="#invariant-{{key}}">\r\n                    <div class="text-left" style="display:flex; align-items:center;">\r\n                        <span class="glyphicon glyphicon-warning-sign"></span>\r\n                        <div marked="val.ruleMessage" style="display:inline-block; margin: 0px 10px;"></div> <!-- uses angular-marked directive -->\r\n                        <span class="badge" style="margin-left:auto;">{{val.tuples.length}}</span>\r\n                    </div>\r\n                </div>\r\n                <ul class="list-group collapse" id="invariant-{{key}}">\r\n                    <li class="list-group-item" ng-repeat="tuple in val.tuples track by $index">\r\n                        <span>{{tuple.violationMessage}}</span>\r\n                    </li>\r\n                </ul>\r\n            </div>\r\n        </div>\r\n                    \r\n        <div id="signals" ng-show="localStorage.notify_showSignals">\r\n            <div class="panel panel-warning" ng-repeat="(key, val) in notifications.signals">\r\n                <div class="panel-heading btn btn-block" data-toggle="collapse" data-target="#violation-{{key}}">\r\n                    <div class="text-left" style="display:flex; align-items:center;">\r\n                        <span class="glyphicon glyphicon-warning-sign"></span>\r\n                        <div marked="val.message" style="display:inline-block; margin: 0px 10px;"></div> <!-- uses angular-marked directive -->\r\n                        <span class="badge" style="margin-left:auto;">{{val.violations.length}}</span>\r\n                    </div>\r\n                </div>\r\n                <ul class="list-group collapse" id="violation-{{key}}">\r\n                    <li class="dropdown list-group-item" ng-repeat="violation in val.violations track by $index">\r\n                        <div ng-if="violation.ifcs.length > 1">\r\n                            <a href="" class="dropdown-toggle" data-toggle="dropdown">{{violation.message}}</a>\r\n                            <ul class="dropdown-menu" role="menu">\r\n                                <li ng-repeat="ifc in violation.ifcs">\r\n                                    <a ng-href="{{ifc.link}}" data-toggle="collapse" data-target="#violation-{{key}}"><small>View</small> {{ifc.label}}</a>\r\n                                </li>\r\n                            </ul>\r\n                        </div>\r\n                        <a ng-if="violation.ifcs.length == 1" ng-href="{{violation.ifcs[0].link}}" data-toggle="collapse" data-target="#violation-{{key}}">{{violation.message}}</a>\r\n                        <span ng-if="violation.ifcs.length == 0">{{violation.message}}</span>\r\n                    </li>\r\n                </ul>\r\n            </div>\r\n        </div>\r\n        \r\n        <!-- Success notifications must be last in notifications center because of position:absolute -->\r\n        <div id="successes" ng-show="localStorage.notify_showSuccesses">\r\n            <div class="alert alert-success alert-dismissible" role="alert" ng-repeat="success in notifications.successes">\r\n                <button type="button" class="close" data-dismiss="alert" aria-label="Close" ng-click="closeAlert(notifications.successes, $index);"><span aria-hidden="true">&times;</span></button>\r\n                <span class="glyphicon glyphicon-ok-sign"></span><span> {{success.message}}</span>\r\n            </div>\r\n        </div>\r\n    </div>\r\n</div>');
$templateCache.put('app/src/shared/404.html','<!-- 404 page -->\r\n<div class="container-fluid" id="Interface">\r\n    <div class="row">\r\n        <div class="col-md-4">\r\n            <h1>404 Page not found</h1>\r\n            <p>The requested page does not exist.</p>\r\n            <p><a class="btn btn-primary btn-lg" href="#/" role="button">Goto startpage</a></p>\r\n        </div>\r\n        <div>\r\n            <img src="app/images/404-image.png">\r\n        </div>\r\n    </div>\r\n</div>');
$templateCache.put('app/src/shared/home.html','<!-- Home screen -->\r\n<div class="container-fluid" id="Interface">\r\n    <div class="jumbotron">\r\n        <h1>Hello, world!</h1>\r\n        <p>You\'ve successfully generated your Ampersand prototype.</p>\r\n        <p><a class="btn btn-primary btn-lg" href="https://ampersandtarski.gitbooks.io/documentation" target="_blank" role="button">See our documentation &raquo;</a></p>\r\n    </div>\r\n</div>\r\n');
$templateCache.put('app/src/shared/loading/loading.html','<img src="app/images/loading.gif" alt="Loading..." style="height:20px;"/>');
$templateCache.put('app/src/shared/myNavTo/myNavToInterfaces.html','<div ng-if="resource._ifcs_.length > 1" style="position:relative; display:inline-block;">\r\n    <a class="dropdown-toggle" data-toggle="dropdown"><ng-transclude></ng-transclude></a>\r\n    <ul class="dropdown-menu" role="menu">\r\n        <li ng-repeat="ifc in resource._ifcs_">\r\n            <a ng-href="#/{{ifc.id}}/{{resource._id_}}" target="{{target}}">{{ifc.label}}</a>\r\n        </li>\r\n    </ul>\r\n</div>\r\n<a ng-if="resource._ifcs_.length == 1" ng-href="#/{{resource._ifcs_[0].id}}/{{resource._id_}}" target="{{target}}"><ng-transclude></ng-transclude></a>\r\n<span ng-if="resource._ifcs_.length == 0 || resource._ifcs_ == undefined"><ng-transclude></ng-transclude></span>');
$templateCache.put('app/src/shared/myNavTo/myNavToOtherInterfaces.html','<!-- Only when ifcs has more than 1 interface, otherwise, the user is already there -->\r\n<!-- This menu includes the interface where the user currently is -->\r\n<div ng-if="resource._ifcs_.length > 1" style="position:relative; display: inline-block;">\r\n    <button type="button" class="btn btn-xs dropdown-toggle" data-toggle="dropdown">\r\n        <span class="glyphicon glyphicon-menu-hamburger"></span>\r\n    </button>\r\n    <ul class="dropdown-menu dropdown-menu-right" role="menu">\r\n        <li ng-repeat="ifc in resource._ifcs_">\r\n            <a ng-href="#/{{ifc.id}}/{{resource._id_}}" target="{{target}}">{{ifc.label}}</a>\r\n        </li>\r\n    </ul>\r\n</div>');}]);
//# sourceMappingURL=ampersand.js.map
