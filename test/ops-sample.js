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
