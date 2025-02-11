// |reftest| skip -- BigInt is not supported
// Copyright (C) 2017 Robin Templeton. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-bigint.prototype.tostring
description: BigInt.prototype.toString.length property descriptor
info: >
  BigInt.prototype.toString ( [ radix ] )

  17 ECMAScript Standard Built-in Objects
includes: [propertyHelper.js]
features: [BigInt]
---*/

verifyProperty(BigInt.prototype.toString, "length", {
  value: 0,
  writable: false,
  enumerable: false,
  configurable: true
});

reportCompare(0, 0);
