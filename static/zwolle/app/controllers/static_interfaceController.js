AmpersandApp.controller('static_interfaceController', function ($scope, $rootScope, $q, Restangular, $timeout, $localStorage, $sessionStorage) {	
	$scope.$localStorage = $localStorage;
	$scope.$sessionStorage = $sessionStorage;
	
	/**********************************************************************************************
	 * 
	 *	CRUD functions on resources
	 * 
	 *********************************************************************************************/
	
	// Function to create (POST) a new resource and add to the colletion
	$scope.createResource = function (obj, ifc, prepend, requestType){
		var deferred = $q.defer();
		
		if(prepend === 'undefined') var prepend = false;
		requestType = requestType || $rootScope.defaultRequestType; // set requestType. This does not work if you want to pass in a falsey value i.e. false, null, undefined, 0 or ""
		if(!Array.isArray(obj['_loading_'])) obj['_loading_'] = new Array();
		
		obj['_loading_'].push( // shows loading indicator
			Restangular.one(obj['_path_']).all(ifc)
				.post({}, {requestType : requestType})
				.then(function(data){					
					// Update visual feedback (notifications and buttons)
					$rootScope.updateNotifications(data.notifications);
					processResponse(data.content, data.invariantRulesHold, data.requestType); // Show/hide buttons on top level resource
					
					// Add new resource to collection/list
					if(!Array.isArray(obj[ifc])) obj[ifc] = new Array();
					if(prepend) obj[ifc].unshift(data.content);
					else obj[ifc].push(data.content);
					
					deferred.resolve(data);
				}, function(reason){
					deferred.reject(reason);
				})
		);
		
		return deferred.promise;
	};
	
	// Function to get (GET) a resource
	$scope.getResource = function (resource, ifc, forceList){
		var deferred = $q.defer();
		
		if(forceList === 'undefined') var forceList = false;
		if(!Array.isArray(resource['_loading_'])) resource['_loading_'] = new Array();
		
		resource['_loading_'].push( // shows loading indicator
			Restangular.one(resource['_path_'])
				.getList(ifc, {forceList : forceList})
				.then(function(data){
					if($.isEmptyObject(data.plain())){
						$rootScope.addInfo('No results found');
					}else{
						resource[ifc] = data;	
					}
					deferred.resolve(data);
				}, function(reason){
					deferred.reject(reason);
				})
		);
		
		return deferred.promise;
	}
	
	// Function to cancel edits and reset (get) resource data
	$scope.cancelResource = function(resource){
		var deferred = $q.defer();
		
		if(!Array.isArray(resource['_loading_'])) resource['_loading_'] = new Array();
		resource['_loading_'].push( // shows loading indicator
			Restangular.one(resource['_path_'])
				.get()
				.then(function(data){
					// Update resource data
					$.extend(resource, data);
							
					// Update visual feedback (notifications and buttons)
					$rootScope.getNotifications(); // get notification again
					initResourceMetaData(resource);
					
					deferred.resolve(data);
				}, function(reason){
					deferred.reject(reason);
				})
		);
		
		return deferred.promise;
	};
	
	/**********************************************************************************************
	 *
	 * Helper functions
	 *
	 **********************************************************************************************/
	
	// Init/reset resource meta data
	initResourceMetaData = function (resource){
		resource['_showButtons_'] = {'save' : false, 'cancel' : false};
		resource['_patchesCache_'] = [];
		setResourceStatus(resource, 'default');
	}
	
	// Process response
	function processResponse(resource, invariantRulesHold, requestType){
		
		if(invariantRulesHold && requestType == 'feedback'){
			resource['_showButtons_'] = {'save' : true, 'cancel' : true};
			setResourceStatus(resource, 'warning');
			
		}else if(invariantRulesHold && requestType == 'promise'){
			resource['_showButtons_'] = {'save' : false, 'cancel' : false};
			resource['_patchesCache_'] = []; // empty patches cache
			
			setResourceStatus(resource, 'success'); // Set status to success
			$timeout(function(){ // After 3 seconds, reset status to default
				setResourceStatus(resource, 'default');
			}, 3000);
		}else{
			resource['_showButtons_'] = {'save' : false, 'cancel' : true};
			setResourceStatus(resource, 'danger');
		}
	};
	
	function setResourceStatus(resource, status){
		// Reset all status properties
		resource['_status_'] = { 'warning' : false
							   , 'danger'  : false
							   , 'default' : false
							   , 'success' : false
							   };
		// Set new status property
		resource['_status_'][status] = true;
	};
	
	$scope.noPendingPromises = function (arr){
		if(!Array.isArray(arr)) return true; // empty array contains no pending promises
		
		return arr.every(function(val){
			return val.$$state.status > 0; // promise status: 0 -> pending, 1 -> resolved, 2 -> rejected
		});
	}
});