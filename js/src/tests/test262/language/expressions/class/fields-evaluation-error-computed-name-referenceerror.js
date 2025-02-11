// |reftest| skip -- class-fields-public is not supported
// This file was procedurally generated from the following sources:
// - src/class-fields/computed-name-referenceerror.case
// - src/class-fields/class-evaluation-error/cls-expr.template
/*---
description: ReferenceError evaluating a computed property name (field definitions in a class expression)
esid: sec-runtime-semantics-classdefinitionevaluation
features: [computed-property-names, class, class-fields-public]
flags: [generated]
info: |
    Runtime Semantics: ClassDefinitionEvaluation

    ...
    27. For each ClassElement e in order from elements
      a. If IsStatic of me is false, then
        i. Let fields be the result of performing ClassElementEvaluation for e with arguments proto and false.
      b. Else,
        i. Let fields be the result of performing ClassElementEvaluation for e with arguments F and false.
      c. If fields is an abrupt completion, then
        i. Set the running execution context's LexicalEnvironment to lex.
        ii. Set the running execution context's PrivateNameEnvironment to outerPrivateEnvironment.
        iii. Return Completion(status).
    ...

    Runtime Semantics: ClassElementEvaluation

    ClassElement: static FieldDefinition;
      Return ClassFieldDefinitionEvaluation of FieldDefinition with parameter true and object.

    ClassElement: FieldDefinition;
      Return ClassFieldDefinitionEvaluation of FieldDefinition with parameter false and object.

    Runtime Semantics: ClassFieldDefinitionEvaluation
      With parameters isStatic and homeObject.

    1. Let fieldName be the result of evaluating ClassElementName.
    2. ReturnIfAbrupt(fieldName).
    ...

    Runtime Semantics: Evaluation
      ComputedPropertyName: [ AssignmentExpression ]

    1. Let exprValue be the result of evaluating AssignmentExpression.
    2. Let propName be ? GetValue(exprValue).
    3. Return ? ToPropertyKey(propName).

---*/
function fn() {
  throw new Test262Error();
}



function evaluate() {
  var C = class {
    [noRef] = fn();
  };
}

assert.throws(ReferenceError, evaluate);

reportCompare(0, 0);
