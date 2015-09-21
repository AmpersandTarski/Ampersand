(function(window, document) {

angular.module('siTable.directives', []);
angular.module('siTable.filters', []);
angular.module('siTable',
[
  'siTable.directives',
  'siTable.filters'
]);
/**
* siSortable Directive
*
* This replaces all TR elements, which is necessary to make the API as non-
* intrusive as possible. It looks for an `ngRepeat` attribute, then adds
* sorting and pagination.
*/
angular.module('siTable.directives').directive('siSortable', function() {
  return {
    restrict: 'A',
    priority: 1001,
    require: '?^siTable',
    scope: true,
    compile: function(tElement, tAttrs) {
      tAttrs.ngRepeat += " | orderBy:sortingParams.sortArray";
      tAttrs.ngRepeat += ' | siPagination:paginationParams';

      return function link(scope, element, attrs, controller) {
        if (!controller) {
          return;
        }

        scope.paginationParams = controller.paginationParams;

        if (attrs.ngRepeat) {
          var matches = attrs.ngRepeat.match(/^\s*([\s\S]+?)\s+in\s+([\s\S]+?)(?:\s+track\s+by\s+([\s\S]+?))?\s*$/),
            collection = matches[2].split('|')[0].trim();
          scope.$watchCollection(collection, function() {
            scope.paginationParams.offset = 0;
          });
        }

        scope.$watch('paginationParams.remote', function(remote) {
          if (remote) {
            scope.sortingParams = {};
          } else {
            scope.sortingParams = controller.sortingParams;
          }
        });
      };
    }
  };
});
/**
* SiTable (main) Directive
*
* The directive adds a controller to the table element, which can be used in
* child directives for communication.
*/
angular.module('siTable.directives').directive('siTable', function() {
  return {
    restrict: 'A',
    scope: {
      sorting: '=?'
    },
    controller: function($scope) {
      var self = this;

      this.paginationParams = {
        offset: 0,
        limit: Infinity,
        remote: false,
        firstText: 'First',
        lastText: 'Last',
        previousText: 'Previous',
        nextText: 'Next'
      };

      this.sortingParams = {
        sortArray: []
      };

      // Copy sortArray to scope binding
      $scope.$watch(function() {
        return self.sortingParams.sortArray;
      }, function(sortArray) {
        if (!angular.isArray($scope.sorting)) {
          $scope.sorting = [];
        }
        $scope.sorting.splice(0, $scope.sorting.length);
        for (var i = 0; i < sortArray.length; i++) {
          $scope.sorting.push(sortArray[i]);
        }
      }, true);
    }
  };
});
/**
* Pagination Directive
*
* Put this somewhere in side the table to add pagination to the table data.
*
*     <table si-table>
*        ...
*       <tfoot>
*         <tr>
*           <td colspan="5">
*             <si-table-pagination limit="10" />
*           </td>
*         </tr>
*       </tfoot>
*     </table>
*
* The directive accepts the following attributes:
*
*  - `limit`: The number of items to show in any one page, at the maximum.
*  - `offset`: (two-way binding) The offset from 0 which is currently showed.
* This variable can be watched from the outside in order to load new items,
* for instance form an API endpoint.
*  - `total`: The tatal number of items available, over all pages. Use this
* attribute to inform the pagination directive about how many pages should be
* displayed in the paginator. NOTE: setting this attribute disables siTable's
* way of paginating. Only use it if the items for any one page is loaded
* asynchronously.
*/
angular.module('siTable.directives').directive('siTablePagination', function() {
  return {
    restrict: 'AE',
    require: '^siTable',
    scope: {
      offset: '=?' // read-only
    },
    template: '\
    <ul class="pagination" ng-show="params.total > params.limit">\
    <li ng-class="{disabled: params.offset === 0}">\
    <a href ng-click="setPage(1)">{{ params.firstText }}</a>\
    </li>\
    <li ng-class="{disabled: params.offset === 0}">\
    <a href ng-click="setPage(currPage - 1)">{{ params.previousText }}</a>\
    </li>\
    <li ng-repeat="page in showPages"\
    ng-class="{active: currPage === page}">\
    <a href ng-click="setPage(page)">{{ page }}</a>\
    </li>\
    <li ng-class="{disabled:\
      params.offset + params.limit >= params.total}">\
      <a href ng-click="setPage(currPage + 1)">{{ params.nextText }}</a>\
      </li>\
      <li ng-class="{disabled:\
        params.offset + params.limit >= params.total}">\
        <a href ng-click="setPage(maxPage)">{{ params.lastText }}</a>\
        </li>\
        </ul>',
    link: function(scope, element, attrs, controller) {
      var indices = 10;

      // Copy parameters from controller. Since we're copying a reference,
      // the two objects stay in sync.
      scope.params = controller.paginationParams;

      // Go to specific page, but only if it is a valid page based on the
      // current parameters in `param`.
      scope.setPage = function(page) {
        if (page >= 1 && page <= scope.maxPage) {
          scope.params.offset = (page - 1) * scope.params.limit;
        }
      };

      // Create a sliding window of pages around the current page. There
      // should always be `params.maxShowPages` page numbers showing.
      scope.$watch('params', function(params) {
        var curr = Math.floor(params.offset / params.limit),
        max = Math.ceil(params.total / params.limit) - 1;

        var minIndex = Math.max(0, Math.min(
          curr - Math.ceil(indices / 2),
          max - indices + 1));

        var showPages = [];
        for (var i = minIndex, count = 0; i <= max && count < indices; count++) {
          showPages.push(i + 1);
          i++;
        }

        // Update variables for use in template
        scope.maxPage = max + 1;
        scope.currPage = curr + 1;
        scope.showPages = showPages;

        // Update API variables
        scope.offset = params.offset;
      }, true);

      // If the total number of items changes, revert the offset to 0.
      // The main use is for filtering, where the user would expect this
      // behavior.
      scope.$watch('params.total', function() {
        scope.params.offset = 0;
      });

      // Watch the `total` attribute for external changes. Setting the
      // total explicitly puts the table in 'remote' mode, meaning it
      // it should display all the items available, and an external
      // mechanism should change the items when another page is selected.
      attrs.$observe('total', function(total) {
        if (!isNaN(parseInt(total, 10))) {
          scope.params.total = parseInt(total, 10);
          scope.params.remote = true;
        }
      });

      // Watch the `limit` attribute for external changes. The parameter
      // is used to decide who many items are shown in a page.
      attrs.$observe('limit', function(limit) {
        if (!isNaN(parseInt(limit, 10))) {
          scope.params.limit = parseInt(limit, 10);
        }
      });

      // Watch the `indices` attribute for external changes. The parameter
      // is used to decide how many indices is shown in the paginator at
      // maximum.
      attrs.$observe('indices', function(_indices) {
        if (!isNaN(parseInt(_indices, 10))) {
          indices = parseInt(_indices, 10);
        }
      });

      // Watch the `firstText` attribute for external changes. The
      // parameter is used as text for First page button
      attrs.$observe('firstText', function(firstText) {
        if (firstText) {
          scope.params.firstText = firstText;
        }
      });

      // Watch the `lastText` attribute for external changes. The
      // parameter is used as text for Last page button
      attrs.$observe('lastText', function(lastText) {
        if (lastText) {
          scope.params.lastText = lastText;
        }
      });

      // Watch the `previousText` attribute for external changes. The
      // parameter is used as text for Previous page button
      attrs.$observe('previousText', function(previousText) {
        if (previousText) {
          scope.params.previousText = previousText;
        }
      });

      // Watch the `nextText` attribute for external changes. The
      // parameter is used as text for Next page button
      attrs.$observe('nextText', function(nextText) {
        if (nextText) {
          scope.params.nextText = nextText;
        }
      });
    }
  };
});
/**
* SortBy Directive
*
* Add sorting interface to TH elements which have the `sortBy` attribute on
* them. The directive accepts the parameter `sortBy`, which should correspond
* to the key on the table items on which to sort.
*
* Suppose the items look like this:
*
*     {
*       name: 'Mikael',
*       age: 25
*     }
*
* then use the `sortBy attribute like the following, in order to make the name-
* column sortable:
*
*     <th sortBy="name">Name</th>
*
* The directive is required to be on a child of an siTable, as it communicates
* with the siTable controller.
*/
angular.module('siTable.directives').directive('sortBy', function() {
  return {
    restrict: 'A',
    transclude: true,
    replace: true,
    scope: true,
    require: '^siTable',
    template: '\
    <th class="sort" ng-click="sort()" ng-class="{\
      \'sort-asc\': state === \'asc\',\
      \'sort-desc\': state === \'desc\'\
    }">\
    <a href ng-transclude></a>\
    <span class="sort-caret sort-asc"\
    ng-if="state === \'desc\'">&#9660;</span>\
    <span class="sort-caret sort-desc"\
    ng-if="state === \'asc\'">&#9650;</span>\
    </th>',
    link: function(scope, element, attrs, controller) {

      // Copy the sorting parameters from the siTable controller. Since
      // we're copying a reference, the parameters will stay in sync.
      var params = controller.sortingParams;

      // Observe the value of the `sortBy` attribute and update the
      // internal model
      attrs.$observe('sortBy', function(sortBy) {
        scope.sortBy = sortBy;
      });

      // If the sortInit attribute is set, then initialize sorting on this
      // header
      if (attrs.sortInit === 'desc') {
        params.sortArray.push('-' + attrs.sortBy);
        scope.state = 'desc';
      } else if (attrs.sortInit) {
        params.sortArray.push(attrs.sortBy);
        scope.state = 'asc';
      }

      scope.sort = function() {
        var sortBy = attrs.sortBy;
        if (!sortBy || !params) {
          return;
        }

        // Tri-state: ascending -> descending -> neutral, represented by
        // an array as per Angular's orderBy specification.
        if (params.sortArray.indexOf(sortBy) !== -1) {
          // ascending -> descending
          params.sortArray[params.sortArray.indexOf(sortBy)] = '-' +
            sortBy;
          scope.state = 'desc';
        } else if (params.sortArray.indexOf('-' + sortBy) !== -1) {
          // descending -> neutral
          params.sortArray.splice(params.sortArray.indexOf('-' +
            sortBy), 1);
          scope.state = '';
        } else {
          // neutral -> ascending
          params.sortArray.push(sortBy);
          scope.state = 'asc';
        }
      };

    }
  };
});
/**
* Pagination filter
*
* Responsible for making sure we display only the items which the pagination
* params dictate.
*
* NOTE: The filter also *writes* to `params.total`, sniffing out the total
* numbers of items before pagination, which is useful for generating the
* pagination directive. It only does so if the `remote` parameter on the
* pagniation parameters is not set.
*/
angular.module('siTable.filters').filter('siPagination', function() {
  return function(input, params) {
    if (!params || params.remote) {
      return input;
    }
    if (input) {
      params.total = input.length;
    }
    return input && input.length ?
      input.slice(params.offset, params.offset + params.limit) : [];
  };
});
})(window, document);