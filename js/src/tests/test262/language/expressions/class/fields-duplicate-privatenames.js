// |reftest| skip error:SyntaxError -- class-fields-private is not supported
// Copyright 2017 Valerie Young.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
description: Syntax error if the same private field defined twice
esid: sec-class-definitions-static-semantics-early-errors
features: [class, class-fields-private]
negative:
  phase: early
  type: SyntaxError
info: |
  Static Semantics: Early Errors

    ClassBody : ClassElementList
    It is a Syntax Error if PrivateBoundNames of ClassBody contains any duplicate entries.
---*/


throw "Test262: This statement should not be evaluated.";

var C = class {
  #x;
  #x;
}
