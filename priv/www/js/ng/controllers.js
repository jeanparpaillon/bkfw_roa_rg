/*global angular */

'use strict';

angular.module('bkfwApp.controllers', [])

.controller('globalCtrl', ['$timeout', 'mcu', function($timeout, mcu) {

  this.mcu = mcu;

}])

.controller('mcuCtrl', ['$scope', '$timeout', '$stateParams', 'mcu', 'dialogs', function($scope, $timeout, $stateParams, mcu, dialogs) {

  this.mode = mcu.mode;
  this.modeID = mcu.modeID;

  // default values
  this.controlMode = mcu.mode.OFF;
  this.controlValue = null;

  this.detail = mcu.api.get({}, {index: $stateParams.mcuIndex},
                            angular.bind(this, function() {
                              // on get()
                              this.controlMode = this.detail.operatingMode;
                              console.debug(this.detail);
                            }));

  this.setOperatingMode = function() {
    console.debug("Setting operating mode...");

    dialogs.confirm("Are you sure ?")

    .then(angular.bind(this, function() {
        console.debug("Setting operating mode " + this.controlMode);
    }));
  };

}])

.controller('systemCtrl', ['sys', function(sys) {

  this.network = sys.net.get();

  this.password = "foo";

  this.community = sys.community.get();

  this.protocol = sys.protocol.get();

  this.firmware = sys.firmware.get();

}])

.controller('loginCtrl', ['$state', 'session', function($state, session) {

  if (session.connected)
    $state.go('dashboard');

  this.user = "admin";
  this.password = null;
  this.error = "";
  this.connecting = false;

  this.connect = function() {
    if (this.user && this.password) {
      // do auth
      session.connect(this.user, this.password)
      .then(
        function() {
          $state.go('dashboard');
        },
        angular.bind(this, function(error) {
          this.error = error;
        }),
        angular.bind(this, function(status) {
          if (status == session.status.CONNECTING)
            this.connecting = true;
          else
            this.connecting = false;
        })
      );
    }
    else {
      this.error = "Missing user or password";
    }
  };

}])

.controller('navCtrl', ['session', function(session) {

  this.session = session;

}]);
