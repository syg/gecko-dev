function makeIterator() {
  var iterator = {
    next: function() {
      print("iter.next");
      return { value: 42, done: false };
    },
    return: function() { print("iter.return"); return {}; }
  };

  return function() { return iterator; };
}

function f() {
  var iterable = {};
  iterable[Symbol.iterator] = makeIterator();

  for (var i of iterable)
    break;
}

dis(f);
f();
