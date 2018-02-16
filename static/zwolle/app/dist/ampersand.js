// when using minified angular modules, use module('myApp', []).controller('MyController', ['myService', function (myService) { ...
angular.module('AmpersandApp', ['ngResource', 'ngRoute', 'ngSanitize', 'restangular', 'ui.bootstrap', 'uiSwitch', 'cgBusy', 'siTable', 'ng-code-mirror', 'ngStorage', 'angularFileUpload', 'ui.bootstrap.datetimepicker', 'hc.marked'])
.config(["$routeProvider", function($routeProvider) {
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
		if(data.navTo != null) $location.url(data.navTo);
        
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
    
    
}]).value('cgBusyDefaults',{
    message:'Loading...',
    backdrop: true,
    //templateUrl: 'my_custom_template.html',
    //delay: 500, // in ms
    minDuration: 500, // in ms
    // wrapperClass: 'my-class my-class2'
}).directive('myShowonhoverBox', function (){
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
}).directive('myBluronenter', function() {
    return function(scope, element, attrs) {
        element.bind("keydown keypress", function(event) {
            if(event.which === 13) { // 13 = Carriage return
                event.target.blur();

                event.preventDefault();
            }
        });
    };
}).filter('toArray', function() {
    // used from: https://github.com/petebacondarwin/angular-toArrayFilter
    return function (obj, addKey) {
        if (!obj) return obj;
        if (Array.isArray(obj)) return obj; // obj is already an array
        if ( addKey === false ) {
          return Object.keys(obj).map(function(key) {
            return obj[key];
          });
        } else {
          return Object.keys(obj).map(function (key) {
            return Object.defineProperty(obj[key], '$key', { enumerable: false, value: key});
          });
        }
      };
}).directive('myNavToInterfaces', function(){
    return {
        restrict : 'E',
        scope : {resource : '=', target : '@'}, // '=' => two-way bind, '@' => evaluates string (use {{}} in html) 
        templateUrl : 'app/src/shared/partials/myNavToInterfaces.html',
        transclude : true
    };
}).directive('myNavToOtherInterfaces', function(){
    return {
        restrict : 'E',
        scope  : {resource : '=', target : '@'}, // '=' => two-way bind, '@' => evaluates string (use {{}} in html) 
        templateUrl : 'app/src/shared/partials/myNavToOtherInterfaces.html'
    };
}).filter('unsafe', ["$sce", function($sce){
    return $sce.trustAsHtml;
}]);
// Controller for extension app in navigation bar
angular.module('AmpersandApp').controller('ExecEngineController', ["$scope", "Restangular", "NotificationService", function ($scope, Restangular, NotificationService) {
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
angular.module('AmpersandApp').controller('InstallerController', ["$scope", "Restangular", "NotificationService", "RoleService", function ($scope, Restangular, NotificationService, RoleService) {
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
angular.module('AmpersandApp').controller('AtomicController', ["$scope", "ResourceService", function($scope, ResourceService){
    
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
}]);
angular.module('AmpersandApp').controller('AtomicDateController', ["$scope", "ResourceService", function ($scope, ResourceService) {
    
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
angular.module('AmpersandApp').controller('AtomicTypeAheadController', ["$scope", "Restangular", "ResourceService", function($scope, Restangular, ResourceService){
    
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
                // Adapt in js model
                resource[ifc].push(angular.copy($item));
                
                // Construct patch(es)
                patch = ResourceService.createPatch('add', resource, patchResource, ifc, $item._id_);
                ResourceService.addPatches(patchResource, [patch]);
                
            }else if(resource[ifc] === null){
                // Adapt js model
                resource[ifc] = angular.copy($item);
                
                // Construct patch(es)
                patch = ResourceService.createPatch('replace', resource, patchResource, ifc, $item._id_);
                ResourceService.addPatches(patchResource, [patch]);
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
angular.module('AmpersandApp').controller('AtomicUploadFileController', ["$scope", "FileUploader", "NotificationService", function($scope, FileUploader, NotificationService){
    
    // File uploader stuff
    $scope.FileUploader = new FileUploader({
        alias : 'file', // fieldname as used in $_FILES['file']
        formData : [],
        removeAfterUpload : true,
        autoUpload : true
    });
    
    $scope.FileUploader.onSuccessItem = function(fileItem, response, status, headers){
        NotificationService.updateNotifications(response.notifications);
        
        // Add response content (newly created FileObject) to ifc list in resource
        fileItem.resource[fileItem.ifc].push(response.content);
    };
    
    $scope.FileUploader.onErrorItem = function(item, response, status, headers){
        NotificationService.addError(response.error.message, response.error.code, true);
    };
}]);
angular.module('AmpersandApp').controller('BoxController', ["$scope", "ResourceService", function($scope, ResourceService){
    
    // Function to create a new resource (does a POST)
    $scope.createResource = ResourceService.createResource; // function(resource, ifc, callingObj, prepend)
    
    // Function to save certain attributes changes of a resource (does a PATCH)
    $scope.save = ResourceService.saveResource; // function(resource)
    
    // Function to cancel unsaved edits (does a GET)
    $scope.cancel = ResourceService.cancelResource; // function(resource)
    
    // Function to remove a resource from an interface (list)
    $scope.remove = ResourceService.removeResource; // function(ifc, resource, patchResource)
    
    // Function to delete a resource
    $scope.delete = ResourceService.deleteResource; // function(ifc, resource)
}]);
angular.module('AmpersandApp').controller('InterfaceController', ["$scope", "$location", "ResourceService", function($scope, $location, ResourceService){
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
angular.module('AmpersandApp').service('ResourceService', ["$localStorage", "$timeout", "$location", "Restangular", "NotificationService", function($localStorage, $timeout, $location, Restangular, NotificationService){
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
                
                // Update resource data
                if(resource._isRoot_) resource.get();
                else resource = angular.extend(resource, data.content);
                
                // Update visual feedback (notifications and buttons)
                ResourceService.processResponse(resource, data);

                return resource;
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
         * @param {bool} prepend
         * @returns {Promise}
         */
        createResource : function(resource, ifc, callingObj, prepend){
            if(prepend === 'undefined') prepend = false;
            
            promise = Restangular
            .one(resource._path_).all(ifc)
            .post({}, {})
            .then(function(data){
                data = data.plain();
                // Update visual feedback (notifications and buttons)
                ResourceService.processResponse(callingObj, data);

                newResource = data.content;
                
                // Add new resource to ifc
                if(Array.isArray(resource[ifc])){ // non-uni = list
                    if(prepend) resource[ifc].unshift(newResource);
                    else resource[ifc].push(newResource);
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
         * @param {bool} patchResource
         * @returns {Promise}
         */
        removeResource : function(parent, ifc, resource, patchResource){
            // Adapt js model
            if(Array.isArray(parent[ifc])) parent[ifc].splice(parent[ifc].indexOf(resource), 1); // non-uni = list
            else parent[ifc] = null; // uni = object
            
            // Construct patch(es)
            patch = ResourceService.createPatch('remove', resource, patchResource);
            return ResourceService.addPatches(patchResource, [patch]);
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
            if($localStorage.switchAutoSave) {
                return ResourceService.saveResource(resource);
            } else {
                // Update visual feedback
                ResourceService.setResourceStatus(resource, 'warning');
                resource._showButtons_ = {'save' : true, 'cancel' : true};
                return $q.resolve(resource);
            }
        },

        /**
         * Returns if there are unsaved changes (i.e. patches that are not yet sent to the API)
         * 
         * @returns {bool}
         */
        checkRequired : function(){ 
            updatedResources.reduce(function(prev, item, index, arr){
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
angular.module('AmpersandApp').controller('NavigationBarController', ["$scope", "$route", "Restangular", "$localStorage", "$sessionStorage", "$timeout", "$location", "NotificationService", "RoleService", "NavigationBarService", function ($scope, $route, Restangular, $localStorage, $sessionStorage, $timeout, $location, NotificationService, RoleService, NavigationBarService) {
    
    $scope.$storage = $localStorage;
    $scope.$sessionStorage = $sessionStorage;
    $scope.defaultSettings = NavigationBarService.defaultSettings;
    $scope.loadingNavBar = [];
    $scope.navbar = NavigationBarService.navbar;
    
    $scope.reload = function(){
        $scope.loadingNavBar = [];
        $scope.loadingNavBar.push(NavigationBarService.refreshNavBar());
        $route.reload();
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
    
    $scope.resetSettings = function(){
        // all off
        angular.forEach($scope.$storage.notificationPrefs, 
            function(value, index, obj){
                obj[index] = false;
            }
        );
        $scope.$storage.switchAutoSave = false;
        
        $timeout(function() {
            // reset to default        
            $scope.resetNotificationSettings();
            $scope.resetSwitchAutoSave();
        }, 500);
    };
    
    $scope.resetNotificationSettings = function(){
        $scope.$storage.notificationPrefs = angular.extend($scope.$storage.notificationPrefs, $scope.defaultSettings.notifications);
    };
    
    $scope.resetSwitchAutoSave = function(){
        $scope.$storage.switchAutoSave = $scope.defaultSettings.switchAutoSave;
    };
    
    $scope.loadingNavBar.push(NavigationBarService.refreshNavBar());
}]).directive('myNavbarResize', ["$window", "$timeout", "NavigationBarService", function ($window, $timeout, NavigationBarService) {
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
            resizeNavbar();
        });
        
        // when window size gets changed
        w.bind('resize', function () {        
            resizeNavbar();
        });
        
        // when page loads
        resizeNavbar();
    };
}]);
angular.module('AmpersandApp').service('NavigationBarService', ["Restangular", "$localStorage", "$sessionStorage", "NotificationService", function(Restangular, $localStorage, $sessionStorage, NotificationService){
    let navbar = {};
    let defaultSettings = {};

    let service = {
        navbar : navbar,
        defaultSettings : defaultSettings,

        refreshNavBar : function(){
            return Restangular
            .one('app/navbar')
            .get()
            .then(function(data){
                data = data.plain();

                angular.extend(navbar, data);
                $sessionStorage.session = data.session;
                $sessionStorage.sessionRoles = data.sessionRoles;
                $sessionStorage.sessionVars = data.sessionVars;
                angular.extend(defaultSettings, data.defaultSettings);
                
                // Default settings for notificationPrefs
                if($localStorage.notificationPrefs === undefined){
                    $scope.resetNotificationSettings();
                }
                // Default setting for switchAutoSave
                if($localStorage.switchAutoSave === undefined){
                    $scope.resetSwitchAutoSave();
                }
                
                // Update notifications
                NotificationService.updateNotifications(data.notifications);
            }, function(error){
                // on error
            });
        }
    };
    
    return service;
}]);
angular.module('AmpersandApp').service('RoleService', ["$sessionStorage", "Restangular", function($sessionStorage, Restangular){
    
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
angular.module('AmpersandApp').controller('RoleMenuController', ["$scope", "RoleService", "NavigationBarService", function ($scope, RoleService, NavigationBarService) {
    $scope.toggleRole = function(roleId, set){
        RoleService.toggleRole(roleId, set);
        NavigationBarService.promises.push(
            RoleService.setActiveRoles()
            .then(function(data){
                NavigationBarService.refreshNavBar();
            })
        );
    };
}]);
angular.module('AmpersandApp').service('NotificationService', ["$localStorage", "$sessionStorage", "$timeout", "Restangular", function($localStorage, $sessionStorage, $timeout, Restangular){
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
            
            if($localStorage.notificationPrefs.switchAutoHideSuccesses){
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
angular.module('AmpersandApp').controller('NotificationCenterController', ["$scope", "$route", "Restangular", "$localStorage", "NotificationService", function ($scope, $route, Restangular, $localStorage, NotificationService) {
    
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

//# sourceMappingURL=ampersand.js.map
