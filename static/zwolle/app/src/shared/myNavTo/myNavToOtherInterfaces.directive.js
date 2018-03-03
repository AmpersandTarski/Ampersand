angular.module('AmpersandApp')
.directive('myNavToOtherInterfaces', function(){
    return {
        restrict : 'E',
        scope  : {resource : '=', target : '@'}, // '=' => two-way bind, '@' => evaluates string (use {{}} in html) 
        templateUrl : 'app/src/shared/myNavTo/myNavToOtherInterfaces.html'
    };
})