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

.directive('usbDisabled', ['usbMode', function(usbMode) {

  return {

    restrict: 'A',

    link: function(scope, element, attrs) {

      var toggleElems = function() {
        var attrVal = "";
        if (usbMode.state === true) {
          attrVal = "disabled";
        }
        element.find('select').attr('disabled', attrVal);
        element.find('input').attr('disabled', attrVal);
        element.find('button').attr('disabled', attrVal);
      };

      scope.$on('usb-mode-enabled', toggleElems);
      scope.$on('usb-mode-disabled', toggleElems);
      toggleElems();
    }

  };

}]);
