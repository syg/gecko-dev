// |reftest| skip -- class-fields-public is not supported
// This file was procedurally generated from the following sources:
// - src/class-fields/static-literal-names.case
// - src/class-fields/productions/cls-decl-new-sc-line-generator.template
/*---
description: Static literal property names (field definitions followed by a method in a new line with a semicolon)
esid: prod-FieldDefinition
features: [class, class-fields-public, generators]
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
const fn = function() {}



class C {
  static a; b = 42;
  static c = fn;
  *m() { return 42; }

}

var c = new C();

assert.sameValue(c.m().next().value, 42);
assert.sameValue(c.m, C.prototype.m);
assert.sameValue(Object.hasOwnProperty.call(c, "m"), false);

verifyProperty(C.prototype, "m", {
  enumerable: false,
  configurable: true,
  writable: true,
});

assert.sameValue(Object.hasOwnProperty.call(C.prototype, "a"), false);
assert.sameValue(Object.hasOwnProperty.call(c, "a"), false);

verifyProperty(C, "a", {
  value: undefined,
  enumerable: true,
  writable: true,
  configurable: true
});

assert.sameValue(Object.hasOwnProperty.call(C.prototype, "b"), false);
assert.sameValue(Object.hasOwnProperty.call(C, "b"), false);

verifyProperty(c, "b", {
  value: 42,
  enumerable: true,
  writable: true,
  configurable: true
});

assert.sameValue(Object.hasOwnProperty.call(C.prototype, "c"), false);
assert.sameValue(Object.hasOwnProperty.call(c, "c"), false);

verifyProperty(C, "c", {
  value: fn,
  enumerable: true,
  writable: true,
  configurable: true
});


reportCompare(0, 0);
