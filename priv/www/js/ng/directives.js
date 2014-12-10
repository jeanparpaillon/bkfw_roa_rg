/*global angular */
/*global splitObject */

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
      $scope.infos = [];
      $scope.info = edfa.info.get(function() {
        $scope.infos = splitObject($scope.info);
      });
      $scope.label = edfa.label;
    }]

  };

});
