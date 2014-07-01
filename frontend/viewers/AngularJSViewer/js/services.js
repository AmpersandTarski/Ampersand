var AmpersandServices = angular.module('AmpersandServices', ['ngResource']);

AmpersandServices.factory('InterfaceObject', ['$resource', function($resource){
	var url;
	if( atom === undefined){
		url = 'http://localhost/CB/api/v1/interface/' + interface + '.json';
	}else{
		url = 'http://localhost/CB/api/v1/interface/' + interface + '/atom/' + atom + '.json';
	}
    return $resource(url, {}, {
    	get: {method:'GET'}
    });
  }]);