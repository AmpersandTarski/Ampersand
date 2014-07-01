var AmpersandControllers = angular.module('AmpersandControllers', ['AmpersandServices']);

AmpersandControllers.controller('InterfaceObjectCtrl', ['$scope', 'InterfaceObject', function ($scope, InterfaceObject) {
	$scope.object = InterfaceObject.get();
}]);