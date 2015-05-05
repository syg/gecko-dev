function x() { n; }
function f() {
  try  { x(); } catch(ex) {}
}
var g = newGlobal();
g.parent = this;
g.eval("new Debugger(parent).onExceptionUnwind = function () {};");
enableSPSProfiling();
enableSingleStepProfiling();
f();
f();
f();
