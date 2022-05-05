/*global angular */

'use strict';

angular.module('bkfwApp.controllers', ['uiSwitch'])

.controller('globalCtrl', ['$scope', '$state', 'AUTH_EVENTS', 'mcu', 'session', 'edfa', function($scope, $state, AUTH_EVENTS, mcu, session, edfa) {

    this.mcu = mcu;
    this.session = session;
    this.edfa = edfa;

    this.isLoginPage = function() {
        return $state.is('login');
    };

    $scope.$on(AUTH_EVENTS.logoutSuccess, function() {
        $state.go('dashboard');
    });

}])

.controller('mcuCtrl', ['$http', '$scope', '$stateParams', 'mcu', 'dialogs', function($http, $scope, $stateParams, mcu, dialogs) {

    this.mode = mcu.mode;
    this.modeID = {
        '3': { name: 'CC', valueType: 'mA' },
        '4': { name: 'OFF', valueType: null },
    }
    this.label = mcu.label;

    this.detail = {};
    this.controlMode = null;
    this.controlValue = null;
    this.controlValues = [0.0, 0.0];

    this.inputLossThreshold = null;
    this.outputLossThreshold = null;

    this.applyAllControl = false;
    this.applyAllThreshold = false;

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
                if (this.controlValues[0] === 0.0 && this.controlValues[1] === 0.0) {
                    this.controlValues = [this.detail.ampConsign, this.detail.ampConsign2];
                    console.debug("Control values are " + this.controlValues);
                }

                if (this.inputLossThreshold === null) {
                    this.inputLossThreshold = this.detail.inputLossThreshold;
                }
                if (this.outputLossThreshold === null) {
                    this.outputLossThreshold = this.detail.outputLossThreshold;
                }
                if (this.detail["has_PC_mode"]) {
                    this.modeID['1'] = { name: 'PC', valueType: 'dBm' };
                }
                if (this.detail["has_GC_mode"]) {
                    this.modeID['2'] = { name: 'GC', valueType: 'dB' };
                }
            }
        })
    );

    this.updateControlValue = function() {
        if (this.controlMode == this.mode.CC) {
            this.controlValues = mcu.getControlValue(this.detail, this.controlMode);
        } else {
            this.controlValue = mcu.getControlValue(this.detail, this.controlMode);
        }
    };

    this.setThresholds = function() {
        if (this.applyAllThreshold) {
            dialogs.confirm("Confirm setting thresholds on all amps:")
                .then(angular.bind(this, function() {
                    var infos = {
                        inputLossThreshold: this.inputLossThreshold,
                        outputLossThreshold: this.outputLossThreshold
                    };
                    $http.post('/api/mcu/', infos)
                        .then(angular.bind(this, function() {
                            dialogs.success("Thresholds set on all amps");
                        }));
                }));
        } else {
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
        }
    };

    this.setOperatingMode = function() {
        if (this.applyAllControl) {
            dialogs.confirm("Confirm setting operating mode on all amps:")
                .then(angular.bind(this, function() {
                    var infos = { operatingMode: this.controlMode };
                    if (this.controlMode == this.mode.CC) {
                        infos['ampConsign'] = this.controlValues[0];
                        infos['ampConsign2'] = this.controlValues[1];
                    } else {
                        infos[mcu.getControlValueName(this.controlMode)] = this.controlValue;
                    }
                    $http.post('/api/mcu/', infos)
                        .then(angular.bind(this, function() {
                            dialogs.success("Operating mode set on all amps");
                        }));
                }));

        } else {
            dialogs.confirm("Confirm setting operating mode:")

            .then(angular.bind(this, function() {
                console.debug("Setting operating mode to " + this.controlMode);

                var newMcu = angular.copy(this.detail);
                newMcu.operatingMode = this.controlMode;
                if (this.controlMode == this.mode.CC) {
                    newMcu['ampConsign'] = this.controlValues[0];
                    newMcu['ampConsign2'] = this.controlValues[1];
                } else {
                    newMcu[mcu.getControlValueName(this.controlMode)] = this.controlValue;
                }

                console.debug("Save mcu " + JSON.stringify(newMcu));

                mcu.save(newMcu)

                .then(angular.bind(this, function() {

                    this.controlMode = null;
                    this.controlValue = null;
                    //this.controlValues = [0.0, 0.0];

                    dialogs.success("Consign applied");
                }));
            }));
        }
    };
}])

.controller('systemCtrl', ['$q', '$http', '$timeout', '$state', 'sys', 'usbMode', 'auth', 'uploaders', 'dialogs', 'edfa', 'apiErrorsConfig', '$scope', function($q, $http, $timeout, $state, sys, usbMode, auth, uploaders, dialogs, edfa, apiErrorsConfig, $scope) {

    function getError(response) {}

    this.firmware = sys.firmware.get();

    // fw | cpu | amp
    this.uploader = [];
    this.uploader['fw'] = uploaders.create('fw', 'Web & SNMP Firmware', 60000);
    this.uploader['cpu'] = uploaders.create('cpu', 'Management Module Firmware', 0);
    this.uploader['amp'] = uploaders.create('amp', 'Units Firmware', 0);

    this.network = sys.net.get();

    this.networkSave = function() {

        dialogs.confirm("Are you sure you want to change network settings ?")

        .then(angular.bind(this, function() {
            apiErrorsConfig.intercept = false;
            this.network.$save()

            .then(function() {
                dialogs.modal("Network settings are being applied, please wait.");
                $timeout(function() { return true }, 10000).then(function() {
                    dialogs.close(true);
                    dialogs.success("You should reload the page on the new network address.");
                });
            });
        }));
    };

    this.password = { password: "", confirm: "" };
    this.community = sys.community.get();
    this.usm = sys.usm.get();
    this.protocol = sys.protocol.get();
    this.targets = sys.targets.get();

    this.targetsSave = function() {
        this.targets.$save()
            .then(function() {
                dialogs.success("SNMP targets saved");
            });
    };

    this.securitySave = function() {

        if (this.password.confirm) {
            $http.post('/api/sys/password', { password: this.password.confirm })
                .then(function() {
                    dialogs.modal("Settings are being applied, please wait...");
                    return edfa.waitUntilOnline(3000)
                        .then(function() {
                            dialogs.close(true);
                            auth.disconnect();
                        });
                });
        }
    };

    this.protocolSave = function() {

        dialogs.confirm("Confirm setting protocols")
            .then(angular.bind(this, function() {
                this.protocol.$save()
                    .then(function() {
                        dialogs.modal("Protocol settings are being applied, please wait...");
                        return edfa.waitUntilOnline(10000)
                            .then(function() {
                                dialogs.close(true);
                            });
                    });
            }));
    };

    this.communitySave = function() {

        dialogs.confirm("Confirm applying SNMPv1/v2c settings")
            .then(angular.bind(this, function() {
                this.community.$save()
                    .then(function() {
                        dialogs.modal("SNMPv1/v2c settings are being applied, please wait...");
                        return edfa.waitUntilOnline(3000)
                            .then(function() {
                                dialogs.close(true);
                            });
                    });
            }));
    };

    this.usmSave = function() {

        dialogs.confirm("Confirm applying SNMPv3 settings")
            .then(angular.bind(this, function() {
                this.usm.$save()
                    .then(function() {
                        dialogs.modal("SNMPv3 settings are being applied, please wait...");
                        return edfa.waitUntilOnline(3000)
                            .then(function() {
                                dialogs.close(true);
                            });
                    });
            }));
    };

    this.reboot = function() {

        dialogs.confirm("The device will reboot")

        .then(function() {
            return $http.post('/api/sys/reboot', { reboot: true });
        })

        .then(function() {
            dialogs.modal("Device is rebooting",
                "If the device isn't online in 2 minutes check if the device IP has changed.");
            // wait edfa to come back
            // start polling in 20 secs
            return edfa.waitUntilOnline(10000);
        })

        .then(function() {
            dialogs.close(true);
            dialogs.success("Device is online");
        });

    };

    this.reset = function() {

        dialogs.confirm("The device will be reset to factory defaults")

        .then(function() {
            return $http.post('/api/sys/reset', { reset: true });
        })

        .then(function() {
                dialogs.modal("Device has beeing reseted",
                    "If you can't contact the device check if its IP has changed.");
                return edfa.waitUntilOnline(10000);
            })
            .then(function() {
                dialogs.close(true);
                dialogs.success("Factory settings have been applied.");
            });

    };

    this.usbmode = usbMode.state;

    $scope.$watch(
        angular.bind(this, function() {
            return this.usbmode;
        }),
        angular.bind(this, function(newVal, oldVal) {
            //console.log("switch usb mode: newVal=" + newVal + " oldVal=" + oldVal + " usbMode.state=" + usbMode.state);

            if (newVal == oldVal || newVal == usbMode.state)
                return;

            var message = "Are you sure you want to ";
            var body = null;
            if (newVal) {
                message += "enable USB mode ?";
                body = "Enabling USB mode will temporary disable SNMP alarms and monitoring";
            } else {
                message += "disable USB mode ?";
            }

            dialogs.confirm(message, body)
                .then(
                    function() {
                        if (newVal === true)
                            usbMode.enable();
                        else if (newVal === false)
                            usbMode.disable();
                    },
                    angular.bind(this, function() {
                        this.usbmode = oldVal;
                    })
                );
        }),
        true
    );

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