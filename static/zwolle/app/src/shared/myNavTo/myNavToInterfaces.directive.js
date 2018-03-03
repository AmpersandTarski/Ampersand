angular.module('AmpersandApp')
.directive('myNavToInterfaces', function(){
    return {
        restrict : 'E',
        scope : {resource : '=', target : '@'}, // '=' => two-way bind, '@' => evaluates string (use {{}} in html) 
        templateUrl : 'app/src/shared/myNavTo/myNavToInterfaces.html',
        transclude : true
    };
})