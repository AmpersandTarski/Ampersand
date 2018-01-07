angular.module('uiSwitch', [])

.directive('switch', function(){
  return {
    restrict: 'AE'
  , replace: true
  , transclude: true
  , template: function(element, attrs) {
      var html = '';
      html += '<a href=""';
      html +=   (attrs.ngModel && !attrs.ngClick) ? ' ng-click="' + attrs.ngModel + '=!' + attrs.ngModel + '"' : '';
      html += '>';
      html += '<span';
      html +=   ' class="switch' + (attrs.class ? ' ' + attrs.class : '') + '"';
      html +=   ' ng-class="{ checked:' + attrs.ngModel + ' }"';
      html +=   '>';
      html +=   '<small></small>';
      html += '</span>';
      html += '<span ng-transclude></span>';
      html += '</a>';
      return html;
    }
  }
});