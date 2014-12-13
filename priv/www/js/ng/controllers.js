/*global angular */

'use strict';

angular.module('bkfwApp.controllers', [])

.controller('globalCtrl', ['$scope', '$state', 'AUTH_EVENTS', 'mcu', 'session', function($scope, $state, AUTH_EVENTS, mcu, session) {

  this.mcu = mcu;
  this.session = session;

  this.isLoginPage = function() {
    return $state.is('login');
  };

  $scope.$on(AUTH_EVENTS.logoutSuccess, function() {
    $state.go('dashboard');
  });

}])

.controller('mcuCtrl', ['$scope', '$stateParams', 'mcu', 'dialogs', function($scope, $stateParams, mcu, dialogs) {

  this.mode = mcu.mode;
  this.modeID = mcu.modeID;
  this.label = mcu.label;

  this.controlValue = null;

  this.detail = mcu.api.get({}, {index: $stateParams.mcuIndex},
                            angular.bind(this, function() {
                              // on get()
                              this.controlValue = this.getControlValue();
                            }));

  this.getControlValueName = function() {
    switch (this.detail.operatingMode) {
      case this.mode.PC:
        return 'outputPowerConsign';
      case this.mode.GC:
        return 'gainConsign';
      case this.mode.CC:
        return 'ampConsign';
    }
    return null;
  };

  this.getControlValue = function() {
    if (this.detail.operatingMode != mcu.mode.OFF) {
      return this.detail[this.getControlValueName()];
    }
    return null;
  };

  this.showControlValue = function() {
    this.controlValue = this.getControlValue();
  };

  this.setOperatingMode = function() {

    dialogs.confirm("Are you sure ?")

    .then(angular.bind(this, function() {
        console.debug("Setting operating mode " + this.detail.operatingMode);

        this.detail.$save()

        .then(
          angular.bind(this, function() {
            dialogs.success("Consign applied");
          }),
          angular.bind(this, function(response) {
            dialogs.error("Failed to apply consign", response.data.join(', '));
          })
        );
    }));
  };

}])

.controller('systemCtrl', ['$q', '$http', '$state', 'sys', 'auth', 'FileUploader', 'dialogs', function($q, $http, $state, sys, auth, FileUploader, dialogs) {

  this.firmware = sys.firmware.get();

  this.uploader = new FileUploader({
    url: '/api/sys/firmware'
  });

  this.network = sys.net.get();

  this.networkSave = function() {

    this.network.$save()

    .then(function() {
      dialogs.success("Network settings saved");
    });
  };

  this.password = {password: "", confirm: ""};
  this.community = sys.community.get();
  this.protocol = sys.protocol.get();

  this.securitySave = function() {

    var actions = [
      [this.community.$save(), "Community settings saved", "Failed to save community settings"],
      [this.protocol.$save(), "Protocol settings saved", "Failed to save protocol settings"],
    ];

    // check this.password.confirm because
    // only this field is validated and it's
    // empty if validation fails
    if (this.password.confirm) {
      actions.push(
        [$http.post('/api/sys/password', {password: this.password.confirm}),
         "New password set", "Failed to set password"]
      );
    }

    $q.all(actions.map(function(a) { return a[0]; }))
    .then(function(actionsResults) {
      for (var i=0; i<actionsResults.length; i++) {
        if (actionsResults[i].$resolved === true) {
          dialogs.success(actions[i][1]);
        }
        else {
          dialogs.error(actions[i][2]);
        }
      }
    });
  };


}])

.directive('compareTo', function() {
  return {
    require: "ngModel",
    scope: {
      otherModelValue: "=compareTo"
    },
    link: function(scope, element, attributes, ngModel) {

      ngModel.$validators.compareTo = function(modelValue) {
        console.debug('========');
        console.debug(scope.otherModelValue);
        console.debug(modelValue);
        return modelValue == scope.otherModelValue;
      };

      scope.$watch("otherModelValue", function() {
        ngModel.$validate();
      });
    }
  };
})

.controller('loginCtrl', ['$scope', '$state', 'auth', 'AUTH_EVENTS', function($scope, $state, auth, AUTH_EVENTS) {

  this.user = "admin";
  this.password = null;
  this.error = "";

  this.authenticate = function(user, pass) {
    auth.authenticate(user, pass).then(angular.bind(this, function() {
      this.error = "";
      if ($state.is('login')) $state.go('dashboard');
    }));
  };

  $scope.$on(AUTH_EVENTS.loginFailed, angular.bind(this, function(event, msg) {
    this.error = msg;
  }));

}]);
