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

  this.detail = {};
  this.controlMode = null;
  this.controlValue = null;

    this.inputLossThreshold = null;
    this.outputLossThreshold = null;

  $scope.$watch(
    function() {
      return mcu.get($stateParams.mcuIndex);
    },
    angular.bind(this, function(newVal) {
      if (newVal) {
        this.detail = newVal;
        // setup initial values
        if (this.controlMode === null) {
          this.controlMode = this.detail.operatingMode.toString();
          console.debug("Current control mode is " + this.controlMode);
        }
        if (this.controlValue === null) {
          this.controlValue = mcu.getControlValue(this.detail, this.controlMode);
          console.debug("Control value is " + this.controlValue);
        }

        if (this.inputLossThreshold === null) {
          this.inputLossThreshold = this.detail.inputLossThreshold;
        }	
        if (this.outputLossThreshold === null) {
          this.outputLossThreshold = this.detail.outputLossThreshold;
        }	
      }
    })
  );

  this.updateControlValue = function() {
    this.controlValue = mcu.getControlValue(this.detail, this.controlMode);
  };

  this.setThresholds = function() {
      dialogs.confirm("Confirm setting thresholds:")

	  .then(angular.bind(this, function() {

	      var newMcu = angular.copy(this.detail);
	      delete newMcu.operatingMode;
	      newMcu.inputLossThreshold = this.inputLossThreshold;
	      newMcu.outputLossThreshold = this.outputLossThreshold;

	      mcu.save(newMcu)
	  
		  .then(angular.bind(this, function() {
		      dialogs.success("Thresholds set");
		  }));
	  }));
  };

  this.setOperatingMode = function() {

    dialogs.confirm("Confirm setting operating mode:")

    .then(angular.bind(this, function() {
      console.debug("Setting operating mode to " + this.controlMode);

      var newMcu  = angular.copy(this.detail);
      newMcu.operatingMode = this.controlMode;
      newMcu[mcu.getControlValueName(this.controlMode)] = this.controlValue;

      console.debug("Save mcu " + JSON.stringify(newMcu));

      mcu.save(newMcu)

      .then(angular.bind(this, function() {

        this.controlMode = null;
        this.controlValue = null;

        dialogs.success("Consign applied");
      }));
    }));
  };

}])

    .controller('systemCtrl', ['$q', '$http', '$timeout', '$state', 'sys', 'auth', 'FileUploader', 'dialogs', 'edfa', 'apiErrorsConfig', '$scope', function($q, $http, $timeout, $state, sys, auth, FileUploader, dialogs, edfa, apiErrorsConfig, $scope) {

  function getError(response) {
  }

  this.firmware = sys.firmware.get();

  this.uploader = new FileUploader({
    url: '/api/sys/firmware',
    headers: $http.defaults.headers.common,
    removeAfterUpload: true,
    onBeforeUploadItem: function() {
      dialogs.modal("Firmare is upgrading",
                    "Device should be online is a few minutes");
    },
    onErrorItem: function() {
      dialogs.close(true);
      dialogs.error("Failed to update the firmware");
    },
    onCompleteItem: angular.bind(this, function() {
      edfa.waitUntilOnline(10000)

      .then(function() {
        dialogs.close(true);
        dialogs.success("Firmware updated");
      });
    })
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
  this.targets  = sys.targets.get();

  this.targetsSave = function() {
      this.targets.$save()
	  .then(function() {
	      dialogs.success("SNMP targets saved");
	  });
  };

  this.securitySave = function() {

    var actions = [ [this.protocol.$save(), "Protocol settings saved"] ];

    if ($scope.security.$valid) {
	actions.push([this.community.$save(), "SNMP settings saved"]);
    }

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

  this.reboot = function() {

    dialogs.confirm("The device will reboot")

    .then(function() {
      return $http.post('/api/sys/reboot', {reboot: true});
    })

    .then(function() {
      dialogs.modal("Device is rebooting",
                    "If the device isn't online in 10 minutes check if the device IP has changed.");
      // wait edfa to come back
      // start polling in 20 secs
      return edfa.waitUntilOnline(20000);
    })

    .then(function() {
      dialogs.close(true);
      dialogs.success("Device is online");
    });

  };

  this.reset = function() {

    dialogs.confirm("The device will be reset to factory defaults")

    .then(function() {
      return $http.post('/api/sys/reset', {reset: true});
    })

    .then(function() {
      dialogs.success("Device has beeing reseted",
                      "If you can't contact the device check if its IP has changed.");
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
