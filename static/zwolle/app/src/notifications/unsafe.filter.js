angular.module('AmpersandApp')
.filter('unsafe', function($sce){
    return $sce.trustAsHtml;
});
