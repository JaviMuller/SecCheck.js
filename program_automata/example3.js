// Two evals and two non-dangerous function calls
function f(x, y) {
  var aux_1 = console.log;
  aux_1(x);
  aux_1(y);
  eval(x);
  eval(y);
}

module.exports = f

/*
l1 -> f
l2 -> x
l3 -> y
l4 -> console.log
l5 -> eval
l6 -> __ret_aux_1_x
l7 -> __ret_aux_1_y
l8 -> __ret_eval_x
l9 -> __ret_eval_y
*/
