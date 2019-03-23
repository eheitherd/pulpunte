'use strict';

const execa = require('execa');

exports.execaImpl = function(command, args, options) {
  return execa(command, args, options);
};
