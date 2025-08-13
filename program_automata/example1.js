// Simple eval
function f(x) {
  eval(x);
}

module.exports = f;

/*
l1 -> f
l2 -> x
l3 -> eval
l4 -> __ret_eval
*/
