// Tests annex B.3.5 that introduces a var via direct eval.

var x = "global-x";

// Tests that direct eval works.
function g() {
  x = "g";
  try { throw 8; } catch (x) {
    eval("var x = 42;");
    log += x;
  }
  log += x;
}
g();

assertEq(log, "42g");

if ("reportCompare" in this)
  reportCompare(true, true)
