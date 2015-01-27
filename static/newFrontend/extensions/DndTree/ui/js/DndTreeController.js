var app = angular.module('AmpersandApp');

AmpersandApp.controller('DndTreeController', function ($scope, $rootScope, $routeParams, Restangular) {
	
	// Get JSON data
	url = 'interface/Root/atom';
	Restangular.one(url, $routeParams.atom).get().then(function(data){
		//******* Steps to transform to usable data for dndTree ******
		
		// Take only first resource of resourcelist
		root = data[0];
		
		// Set root name to provided label
		root.name = root.label 
		
		// 
		convertChildren(root);		
		
		console.log(root);
		treeJSON(root);
	});
    
});

function convertChildren(obj){
	
	obj.name = obj.label;
	
	if (obj['children']) {
		obj['children'] = objToArray(obj['children']);
		
		for (val of obj['children'])
			convertChildren(val);
	}
	
}

function objToArray(obj){
	var arr = [];
	for (var key in obj) {
		arr.push(obj[key]);
	}
	return arr;
}