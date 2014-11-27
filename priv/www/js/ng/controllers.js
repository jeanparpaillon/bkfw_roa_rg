/*global angular */

'use strict';

angular.module('bkfwApp.controllers', [])

.controller('globalCtrl', ['$timeout', 'modules', function($timeout, modules) {

  this.modules = modules;

}])

.controller('moduleCtrl', ['$scope', '$timeout', '$stateParams', 'modules', 'dialogs', function($scope, $timeout, $stateParams, modules, dialogs) {

  var controlValueTypes = {
    CC: 'mA',
    GC: 'dB',
    PC: 'dBm'
  };

  this.detail = {};
  this.controlMode = null;
  this.controlValue = null;
  this.controlValueType = null;

  this.setControlMode = function() {
    console.debug("Setting control mode...");

    dialogs.confirm("Are you sure ?")

    .then(angular.bind(this, function() {
        console.debug("Setting control mode " + this.controlMode);
    }));
  };

  modules.detail($stateParams.moduleIndex)
  .then(angular.bind(this, function(value) {
    this.detail = value;
    this.controlMode = this.detail.mode;
  }));

  $scope.$watch(
    angular.bind(this, function() {
      return this.controlMode;
    }),
    angular.bind(this, function(newVal) {
      if (newVal)
        this.controlValueType = controlValueTypes[this.controlMode];
    })
  );

}])

.controller('systemCtrl', [function() {

  this.network = {
    type: "dhcp",
    ip: null,
    netmask: null,
    gateway: null
  };

  this.password = "foo";

  this.community = "bar";

  this.protocols = {
    snmpv1: true,
    snmpv2: false,
    snmpv3: true
  };

  this.firmware = {
    file: null,
    version: "v0.1"
  };

  this.saveNetwork = function() {

  };


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
