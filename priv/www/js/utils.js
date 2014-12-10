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
