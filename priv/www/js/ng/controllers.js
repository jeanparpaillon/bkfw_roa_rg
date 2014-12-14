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

        .then(angular.bind(this, function() {
          dialogs.success("Consign applied");
        }));
    }));
  };

}])

.controller('systemCtrl', ['$q', '$http', '$timeout', '$state', 'sys', 'auth', 'FileUploader', 'dialogs', 'edfa', 'apiErrorsConfig', function($q, $http, $timeout, $state, sys, auth, FileUploader, dialogs, edfa, apiErrorsConfig) {

  function getError(response) {
  }

  this.firmware = sys.firmware.get();

  this.uploader = new FileUploader({
    url: '/api/sys/firmware',
    headers: $http.defaults.headers.common
  });

  this.network = sys.net.get();

  this.networkSave = function() {

    this.network.$save()

    .then(function() {
      dialogs.success("Network settings applied");
    });

  };

  this.password = {password: "", confirm: ""};
  this.community = sys.community.get();
  this.protocol = sys.protocol.get();

  this.securitySave = function() {

    var actions = [
      [this.community.$save(), "Community settings saved"],
      [this.protocol.$save(), "Protocol settings saved"],
    ];

    // check this.password.confirm because
    // only this field is validated and it's
    // empty if validation fails
    if (this.password.confirm) {
      actions.push(
        [$http.post('/api/sys/password', {password: this.password.confirm}),
         "New password set"]
      );
    }

    $q.all(actions.map(function(a) {
      return a[0]
      .then(function() {
        dialogs.success(a[1]);
      });
    }));
  };

  var checkOnline = function() {

    edfa.isOnline()

    .then(function() {
      dialogs.close();
      dialogs.success("Device is online");
      // get future errors
      apiErrorsConfig.intercept = true;
    })

    .catch(function() {
      // check again in 5 seconds
      $timeout(checkOnline, 5000);
    });

  };

  this.reboot = function() {

    dialogs.confirm("The device will reboot")

    .then(function() {
      return $http.post('/api/sys/reboot', {reboot: true});
    })

    .then(function() {
      dialogs.modal("Device is rebooting", "If the device isn't online in 10 minutes check if the device IP has changed.");
    })

    .then(function() {
      // we don't want to catch API errors now
      apiErrorsConfig.intercept = false;
      // wait before checking
      $timeout(checkOnline, 20000);
    });

  };

  this.reset = function() {

    dialogs.confirm("The device will be reset to factory defaults")

    .then(function() {
      return $http.post('/api/sys/reset', {reset: true});
    })

    .then(function() {
      dialogs.modal("Device has beeing reseted", "If you can't contact the device check if its IP has changed.");
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
