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

  // default values
  this.controlMode = mcu.mode.OFF;
  this.controlValue = null;

  this.detail = mcu.api.get({}, {index: $stateParams.mcuIndex},
                            angular.bind(this, function() {
                              // on get()
                              this.controlMode = this.detail.operatingMode;
                            }));

  this.setOperatingMode = function() {

    dialogs.confirm("Are you sure ?")

    .then(angular.bind(this, function() {
        console.debug("Setting operating mode " + this.controlMode);
        this.detail.operatingMode = this.controlMode;

        switch (this.controlMode) {
          case mcu.mode.PC:
            this.detail.outputPowerConsign = this.controlValue;
            break;
          case mcu.mode.GC:
            this.detail.gainConsign = this.controlValue;
            break;
          case mcu.mode.CC:
            this.detail.ampConsign = this.controlValue;
            break;
          default:
            break;
        }

        this.detail.$save(angular.bind(this, function(detail) {
          dialogs.success("Operating mode set to " + mcu.modeID[this.controlMode].name);
        }));

    }));
  };

}])

.controller('systemCtrl', ['$state', 'sys', 'auth', 'FileUploader', function($state, sys, auth, FileUploader) {

  this.firmware = sys.firmware.get();

  this.uploader = new FileUploader({
    url: '/api/sys/firmware'
  });

  this.network = sys.net.get();
  this.password = "";
  this.community = sys.community.get();
  this.protocol = sys.protocol.get();

}])

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

  $scope.$on(AUTH_EVENTS.loginFailed, angular.bind(this, function(msg) {
    this.error = msg;
  }));

}]);
