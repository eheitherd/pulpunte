'use strict'

const chokidar = require('chokidar');

exports.watchImpl = function(files, options, event, handler) {
  const watcher = chokidar.watch(files, options);
  watcher.on(event, function(arg) {
    handler(watcher)(arg)();
  });
  return {};
};

exports.closeImpl = function(watcher) {
  watcher.close();
  return {};
};