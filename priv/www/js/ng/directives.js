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
      $scope.edfa = edfa;
    }]

  };

})

.directive('loginDialog', ['AUTH_EVENTS', function (AUTH_EVENTS) {

  return {

    restrict: 'A',

    template: '<div ng-if="visible"><div class="overlay" ng-click="cancel()"></div><div class="popup alert alert-warning" ng-include="\'partials/login-form.html\'"></div></div>',

    link: function (scope) {

      var showDialog = function () {
        scope.visible = true;
      };
      var hideDialog = function() {
        scope.visible = false;
      };

      scope.visible = false;

      scope.$on(AUTH_EVENTS.notAuthenticated, showDialog);
      scope.$on(AUTH_EVENTS.sessionTimeout, showDialog);
      scope.$on(AUTH_EVENTS.loginSuccess, hideDialog);
    },

    controller: ['$scope', 'auth', function($scope, auth) {
      $scope.cancel = function() {
        // forget about buffered requests...
        auth.cancelAuthenticate();
        $scope.visible = false;
      };
    }]

  };
}])

.directive('usbSwitch', ['dialogs', function(dialogs) {
  return {
    priority: -1,
    restrict: 'A',
    require: 'ngModel',
    link: function(scope, element, attrs, ngModelCtrl) {
      element.bind('click', function(e) {
        var message = "Are you sure you want to ";
        if (ngModelCtrl.$modelValue) {
          message += "enable USB mode ?";
        } else {
          message += "disable USB mode ?";
        }
        if (message && !confirm(message)) {
          ngModelCtrl.$setModelValue(!ngModelCtrl.$modelValue);
          e.stopImmediatePropagation();
          e.preventDefault();
        }
      });
    }
  };
}]);
