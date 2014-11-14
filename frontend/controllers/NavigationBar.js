AmpersandApp.controller('NavigationBarController', ['$scope', '$routeParams', 'InterfaceFactory', 'RoleFactory', 'ExtensionFactory', function ($scope, $routeParams, InterfaceFactory, RoleFactory, ExtensionFactory) {
	$scope.session_id = session_id;
	$scope.Interfaces = InterfaceFactory.get({}, function(){
		
	});
	$scope.Roles = RoleFactory.get({}, function(){
		
	});
	$scope.Extensions = ExtensionFactory.get({}, function(){
		
	});
	
}]);

AmpersandApp.factory('InterfaceFactory', ['$resource', function($resource){
	var url = 'api/v1/interfaces/top.json';
	
	return $resource(url, {}, {
		get: {method:'GET', isArray:true}
	});
}]);

AmpersandApp.factory('RoleFactory', ['$resource', function($resource){
	var url = 'api/v1/roles/all.json';
	
	return $resource(url, {}, {
		get: {method:'GET', isArray:true}
	});
}]);

AmpersandApp.factory('ExtensionFactory', ['$resource', function($resource){
	var url = 'api/v1/extensions/all.json';
	
	return $resource(url, {}, {
		get: {method:'GET', isArray:true}
	});
}]);