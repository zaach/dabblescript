var assert = require('assert');


// test un-declared operator
function concat (a, b) {
  return a.concat(b);
}

var a = [1,2] `concat` [3,4];

assert.deepEqual(a, [1,2,3,4]);

// reassign precedence
infixl + `concat`;

var concat2 = concat;
// test overloaded operator using function identifier
infixl * (<<) = concat2;

var c = [1,2] << [3,4];
assert.deepEqual(c, [1,2,3,4]);

// test new operator (unused syntax) using function definition
infixl * (^) = function (a, b) {
  return Math.pow(a, b);
};

var b = 2 ^ 10;

assert.equal(b, 1024);

var concat3 = concat;
// test new operator using function identifier
// uses number for precedence
infixl 8 (::) = concat3;

var d = [1,2] :: [3,4];
assert.deepEqual(d, [1,2,3,4]);

// test deleting an operation
deleteop <<;
assert.equal( 1 << 2, 4);

(function (hmm) {
  infixl 9 (+) = function (a, b) {
    return a*b;
  };

  assert.equal(1 + 2, 2);
})();

assert.equal(1 + 2, 3);

// range inclusive operator
infixl 8 (..) = function (a, b) {
    var array = [];
    for (;a <= b; a++) {
        array.push(a);
    }
    return array;
};

assert.deepEqual(1..4, [1,2,3,4]);


// test postfix operators
postfix (?) = function (a) {
  return !!a;
};

assert.strictEqual(''?, false);

// test prefix operators
prefix (@) = function (a) {
  return Date.parse(a);
};

assert.equal(@"December 28, 1995", 820137600000);

// test shadowing operator and right associativity
(function () {
  infixr || (@) = function (a, b) {
    return a(b);
  };
  assert.equal(String @ Math.floor @ 3.14, "3");
})();

assert.equal(@"December 28, 1995", 820137600000);

// test escaped paren "\)" and escaped escape "\\"
infixr 5 (\\-\)) = function (a, b) {
  return a - b;
};

assert.equal(5 \-) 4, 1);

