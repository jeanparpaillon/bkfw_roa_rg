/*global angular */

'use strict';

angular.module('bkfwApp.controllers', [])

.controller('dashboardCtrl', ['$timeout', 'modules', function($timeout, modules) {

  this.list = [];

  this.getList = function() {
    modules.list()
    .then(angular.bind(this, function(values) {
      this.list = values;
    }))
    // refresh modules list 3 secs later
    .finally($timeout(angular.bind(this, this.getList), 3000));
  };

  // get modules list now.
  this.getList();


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
