// |reftest| error:SyntaxError
// Copyright (C) 2015 André Bargull. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-generator-function-definitions-static-semantics-early-errors
description: >
  A SyntaxError is thrown if a generator method contains a non-simple parameter list and a UseStrict directive.
info: >
  Static Semantics: Early Errors

  It is a Syntax Error if ContainsUseStrict of GeneratorBody is true and IsSimpleParameterList of StrictFormalParameters is false.
negative:
  phase: early
  type: SyntaxError
features: [generators]
---*/

throw "Test262: This statement should not be evaluated.";

var o = {
  *m(a = 0) {
    "use strict";
  }
};
