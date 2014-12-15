'use strict';

var splitObject = function(obj) {
  var list = [],
      left = {},
      right = {};

  Object.getOwnPropertyNames(obj).sort().forEach(function(k) {
    list.push([k, obj[k]]);
  });

  var middle = Math.ceil(list.length / 2);

  return list.reduce(function(acc, elem, index) {
    if (index <= middle) {
      acc[0][elem[0]] = elem[1];
    }
    else {
      acc[1][elem[0]] = elem[1];
    }
    return acc;
  }, [left, right]);
};

// for unsupported browsers
if (!Array.prototype.findIndex) {
  Array.prototype.findIndex = function(predicate) {
    if (this == null) {
      throw new TypeError('Array.prototype.find called on null or undefined');
    }
    if (typeof predicate !== 'function') {
      throw new TypeError('predicate must be a function');
    }
    var list = Object(this);
    var length = list.length >>> 0;
    var thisArg = arguments[1];
    var value;

    for (var i = 0; i < length; i++) {
      value = list[i];
      if (predicate.call(thisArg, value, i, list)) {
        return i;
      }
    }
    return -1;
  };
}
