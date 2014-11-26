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

.controller('moduleCtrl', ['$timeout', '$stateParams', 'modules', function($timeout, $stateParams, modules) {

  this.detail = {};

  modules.detail($stateParams.moduleIndex)
  .then(angular.bind(this, function(value) {
    this.detail = value;
  }));

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
