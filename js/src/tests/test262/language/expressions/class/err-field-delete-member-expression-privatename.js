// |reftest| skip error:SyntaxError -- class-fields-private,class-fields-public is not supported
// This file was procedurally generated from the following sources:
// - src/class-fields/member-expression-privatename.case
// - src/class-fields/delete-error/cls-expr-field-delete.template
/*---
description: Syntax error if you call delete on member expressions . privatename (in field)
esid: sec-class-definitions-static-semantics-early-errors
features: [class-fields-private, class, class-fields-public]
flags: [generated]
negative:
  phase: early
  type: SyntaxError
info: |
    Static Semantics: Early Errors

      UnaryExpression : delete UnaryExpression

      It is a Syntax Error if the UnaryExpression is contained in strict mode code and the derived UnaryExpression is PrimaryExpression : IdentifierReference , MemberExpression : MemberExpression.PrivateName , or CallExpression : CallExpression.PrivateName .

---*/


throw "Test262: This statement should not be evaluated.";

var C = class {
  #x;
  
  x = delete this.#x;

  
}
