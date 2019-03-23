'use strict'

exports.eval = function(src) {
  return function() {
    return eval(src);
  }
};
