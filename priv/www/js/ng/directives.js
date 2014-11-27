/*global angular */

'use strict';

angular.module('bkfwApp.directives', [])

/** Show loading information in an element */
.directive('bkfwSpinner', function() {
  return {
    restrict: 'A',
    template: '<img src="img/" alt="Loading..." />',
  };
})

.directive('edfaInfo', function() {

  return {

    restrict: 'E',

    replace: true,

    templateUrl: 'partials/edfa.html',

    controller: ['$scope', 'edfa', function($scope, edfa) {

      $scope.info = edfa.get();

    }]

  };

});
