// Two evals and two non-dangerous function calls
function f(x, y) {
  var aux_1 = console.log;
  aux_1(x);
  aux_1(y);
  eval(x);
  eval(y);
}
