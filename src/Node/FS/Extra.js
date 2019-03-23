'use strict';

const fs = require('fs-extra');

exports.outputFileImpl = function(encoding, file, data) {
  return fs.outputFile(file, data, encoding);
};

exports.readJsonImpl = function(file) {
  return fs.readJson(file);
};

exports.writeJsonImpl = function(spaces, file, object) {
  return fs.writeJson(file, object, {spaces: spaces});
};

exports.removeImpl = function(path) {
  return fs.remove(path);
};
