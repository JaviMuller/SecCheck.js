// Prototype pollution inside a loop
// 1st iteration, if entering if, will make new_obj = Object.prototype
// 2nd iteration, if going to else, will make Object.prototype[<tainted>] = <tainted>
function f(obj, ls, val) {
  new_obj = {};
  for(i = 0; true; ++i) {
    aux_1 = ls[i];
    if (Date.now() % 2 == 0) {
      new_obj = obj[aux_1];
    } else {
      new_obj[aux_1] = val;
    }
  }
}
