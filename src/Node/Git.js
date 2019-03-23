'use strict';

const fs = require('fs');
const git = require('isomorphic-git');

git.plugins.set('fs', fs);

exports.gitImpl = function(command, options) {
  return git[command](options);
};
