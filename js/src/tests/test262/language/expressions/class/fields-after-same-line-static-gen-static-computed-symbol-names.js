// |reftest| skip -- class-fields-public is not supported
// This file was procedurally generated from the following sources:
// - src/class-fields/static-computed-symbol-names.case
// - src/class-fields/productions/cls-expr-after-same-line-static-gen.template
/*---
description: Static computed property symbol names (field definitions after a static generator in the same line)
esid: prod-FieldDefinition
features: [Symbol, computed-property-names, generators, class, class-fields-public]
flags: [generated]
includes: [propertyHelper.js]
info: |
    ClassElement:
      ...
      FieldDefinition ;

    FieldDefinition:
      ClassElementName Initializer_opt

    ClassElementName:
      PropertyName

---*/
var x = Symbol();
var y = Symbol();



var C = class {
  static *m() { return 42; } static [x]; static [y] = 42;

}

var c = new C();

assert.sameValue(C.m().next().value, 42);
assert.sameValue(Object.hasOwnProperty.call(c, "m"), false);
assert.sameValue(Object.hasOwnProperty.call(C.prototype, "m"), false);

verifyProperty(C, "m", {
  enumerable: false,
  configurable: true,
  writable: true,
});

assert.sameValue(Object.hasOwnProperty.call(C.prototype, x), false);
assert.sameValue(Object.hasOwnProperty.call(c, x), false);

verifyProperty(C, x, {
  value: undefined,
  enumerable: true,
  writable: true,
  configurable: true
});

assert.sameValue(Object.hasOwnProperty.call(C.prototype, y), false);
assert.sameValue(Object.hasOwnProperty.call(c, y), false);

verifyProperty(C, y, {
  value: 42,
  enumerable: true,
  writable: true,
  configurable: true
});

assert.sameValue(Object.hasOwnProperty.call(C.prototype, "x"), false);
assert.sameValue(Object.hasOwnProperty.call(C, "x"), false);
assert.sameValue(Object.hasOwnProperty.call(c, "x"), false);

assert.sameValue(Object.hasOwnProperty.call(C.prototype, "y"), false);
assert.sameValue(Object.hasOwnProperty.call(C, "y"), false);
assert.sameValue(Object.hasOwnProperty.call(c, "y"), false);

reportCompare(0, 0);
