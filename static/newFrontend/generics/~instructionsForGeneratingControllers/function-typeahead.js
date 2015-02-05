/*
 * Insert code below in controller if the interface has at least 1 editable relation to another concept (i.e. not primitive datatype)
 * 
 */

// Typeahead functionality
$scope.selected = {}; // an empty object for temporary storing typeahead selections
$scope.typeahead = {}; // an empty object for typeahead

// An property for every editable relation to another concept (i.e. not primitive datatypes)
$scope.typeahead['<name in interface>'] = Restangular.all('resource/<tgtConcept>').getList().$object;
// e.g. $scope.typeahead['Theme'] = Restangular.all('resource/Theme').getList().$object;