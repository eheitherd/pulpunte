'use strict'

const which = require('which');

exports.whichImpl = function(executable) {
  return new Promise(function(resolve, reject) {
    which(executable, function(err, resolvedPath) {
      if (err) {
        reject(err);
      } else {
        resolve(resolvedPath);
      }
    });
  });
};
