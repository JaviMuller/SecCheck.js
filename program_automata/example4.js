// Prototype pollution inside a loop
// 1st iteration, if entering if, will make new_obj = Object.prototype
// 2nd iteration, if going to else, will make Object.prototype[<tainted>] = <tainted>
function f(obj, ls, val) {
  new_obj = {};
  for(i = 0; i < 100; ++i) {
    aux_1 = ls[i];
    if (Date.now() % 2 == 0) {
      new_obj = obj[aux_1];
    } else {
      new_obj[aux_1] = val;
    }
  }
}

module.exports = f

/*
l1 -> f
l2 -> obj
l3 -> ls
l4 -> val
l5 -> new_obj@5
l6 -> i@6 (0-99)

CPG:

             f           new_obj@5        i@6
            [l1]            [l5]           [l6]
           / |  \            /
          /  |   \          /
     arg /   |    \ arg    / *
        /    | arg \      /
       /     |      \    /
 obj [l2]  [l3] ls   [l4] val
     |  |\ |  |     /
     +--+ \+--+    /
      *    \ *    /
            \    /
             \  /
              \/
              *

Automaton:

        {l2} := {l2}[{l3}]
  + --------------------------- +
  |                             |
  ∨     {l3} := {l3}[{l6}]      |
[s0] -----------------------> [s1]
  ∧                             |
  |    {l2, l5}[{l3}] := l4     |
  + --------------------------- +

*/              
