assertThrowsInstanceOf(() => evaluate(`try { throw {} } catch ({e}) { var e; }`), SyntaxError);
assertThrowsInstanceOf(() => evaluate(`try { throw {} } catch ({e}) { eval('var e'); }`), SyntaxError);

if (typeof reportCompare === "function")
    reportCompare(true, true);
