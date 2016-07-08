// |reftest| skip-if(!xulRuntime.shell) -- needs evaluate
// Any copyright is dedicated to the Public Domain.
// http://creativecommons.org/licenses/publicdomain/

// Attempting to lexically redefine a var is a syntax error.
evaluate("var a;");
assertThrowsInstanceOf(() => evaluate("let a;"), SyntaxError);

// Attempting to lexically redefine a global property that's not a var is okay.
this.b = 42;
assertEq(b, 42);
evaluate("let b = 17;");
assertEq(b, 17);

// Attempting to lexically redefine a global property that wasn't a var
// initially but was later declared as one, isn't okay.
this.c = 8675309;
assertEq(c, 8675309);
evaluate("var c;");
assertThrowsInstanceOf(() => evaluate("let c;"), SyntaxError);

// Attempting to lexically redefine a var added by eval code isn't okay.
assertEq(typeof d, "undefined");
eval("var d = 33;");
assertEq(d, 33);
assertThrowsInstanceOf(() => evaluate("let d;"), SyntaxError);

// Attempting to lexically redefine a var added by eval code, *then deleted*,
// isn't okay.  (The |var| will add the name to the global environment record's
// [[VarNames]], and it's not removed when the property is deleted.  #loljs)
assertEq(typeof e, "undefined");
eval("var e = 'ohia';");
assertEq(e, "ohia");
delete this.e;
assertEq(this.hasOwnProperty("e"), false);
assertThrowsInstanceOf(() => evaluate("let e;"), SyntaxError);

if (typeof reportCompare === "function")
  reportCompare(true, true);

print("Tests complete");
