'use strict';

angular.module('bkfwApp.utils', [])
    .factory('uploaders', ['$http', '$q', 'FileUploader', 'dialogs', 'edfa', function($http, $q, FileUploader, dialogs, edfa) {
	return {
	    create: function(name, label, wait) {
		this.u = new FileUploader({
		    url: '/api/sys/firmware/' + name,
		    headers: $http.defaults.headers.common,
		    removeAfterUpload: true,
		    onBeforeUploadItem: function() {
			dialogs.modal(label + " is upgrading",
				      "Do not power off or shutdown the device !");
		    },
		    onErrorItem: function() {
			dialogs.close(true);
			dialogs.error("Failed to update " + label);
		    }
		});

		if (wait == 0) {
		    this.u.onCompleteItem = angular.bind(this, function() {
			dialogs.close(true);
			dialogs.success(label + " updated.");
		    });
		} else {
		    this.u.onCompleteItem = angular.bind(this, function() {
			edfa.waitUntilOnline(wait)
			
			    .then(function() {
				dialogs.close(true);
				dialogs.success(label + " updated.");
			    });
		    })
		};
		
		return this.u;
	    }
	};
}])

.factory('dialogs', function($q) {

  var popups = [];

  function Dialog(params) {
    angular.extend(this, params);
    this.result = $q.defer();
    popups.push(this);
  }

  Dialog.prototype = {

    close: function(value) {
      popups.splice(popups.indexOf(this), 1);
      if (value)
        this.result.resolve(value);
      else
        this.result.reject(value);
    },

    canConfirm: function() {
      return ["confirm", "info"].indexOf(this.type) !== -1;
    },

    canAbort: function() {
      return ["confirm"].indexOf(this.type) !== -1;
    }
  };

  return {

    all: function() {
      return popups;
    },

    show: function(type, klass, title, body) {
      return new Dialog({
        type: type,
        klass: klass,
        title: title,
        body: body
      }).result.promise;
    },

    confirm: function(title, body) {
      return this.show('confirm', 'info', title, body);
    },

    success: function(title, body) {
      return this.show('info', 'success', title, body);
    },

    error: function(title, body) {
      return this.show('info', 'danger', title, body);
    },

    modal: function(title, body) {
      return this.show('modal', 'info', title, body);
    },

    close: function(force) {
      var dialog = popups[popups.length - 1];
      if (dialog.type != 'modal' || force === true)
        dialog.close(false);
    }
  };
})

.directive('dialogs', function() {

  return {
    restrict: 'E',
    templateUrl: 'partials/dialogs.html',
    controller: ['$scope', 'dialogs', function($scope, dialogs) {
      $scope.dialogs = dialogs;
    }]
  };

})

.filter('mcuMode', ['mcu', function(mcu) {

  return function(id) {
    id = id || mcu.mode.OFF;
    return mcu.modeID[id].name;
  };

}]);
