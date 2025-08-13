// Simple prototype-pollution
function f(x, y, z) {
  o2 = o1[x];
  o2[y] = z;
}

module.exports = f;

/*
l1 -> f
l2 -> x
l3 -> y
l4 -> z, o1[*]
l5 -> o1, o1[*]
*/