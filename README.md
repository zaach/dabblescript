# DabbleScript

Dabble with JavaScript syntax.

    // define an "is" operator
    infixl 8 (is) = function (a, b) {
      return a === b;
    };

    assert.ok("everything" is "everything");

## Install

    $ npm install dabble

Or clone from git and run `npm install` and then `make`.

## Use

Translate dabble scripts:

    $ dabble exaple/es-next-operators.dabble

## Operator Declarations

Dabblescript lets you define new operators or overwrite existing ones, expressing them as functions. The operators can be infix, postfix, prefix, and they can update a reference or simply return a value. Operators can also be `lazy`, to support short-circuiting. Lazy operators will have their arguments wrapped in thunks, which can evaluated in the operator definition. Precedence determines the order of operations (detailed below), and can be a number from 1 - 16 or a symbol of a builtin operator (to copy that operators precedence).

Dabblescript modifies the lexer during compile time to recognize new operators and replaces uses of them with a call to the function you define for them. Operators are lexically scoped, but without hoisting.

### infixl `[lazy]` `<precedence>` (`op`) = `expr`;

This creates an infix, left-associative operator. Most JavaScript operators are left-associative. Example:

    // strongly-typed addition operator
    infixl 9 (+) = function (a, b) {
      if (typeof a !== 'number' || typeof b !== 'number') {
        throw new Error("TypeError: operands of + must be numbers");
      }
      return a + b;
    };

    assert['throws'](function () { 42 + "oh hai"; });

### infixr `[lazy]` `<precedence>` (`op`) = `expr`;

This creates an infix, right-associative operator. Example:

    // cons operator
    infixr 9 (::) = function (a, b) {
      return [a, b];
    };

### prefix `[lazy]` (`op`) = `expr`;

This creates a prefix operator (e.g. `!` or `void`). Example:

    prefix (@) = function (a) {
      return Date.parse(a);
    };

### postfix `[lazy]` (`op`) = `expr`;

This creates a postfix operator (e.g. `++` or `--`). Example:

    postfix (?) = function (a) {
      return !!a;
    };

### assign `[infixr|prefix|postfix]` (`op`) = `expr`;

This creates an assignment operator with a fixity that is either `infixr`, `prefix`, or `postfix`. If you omit the fixity it will default to `infixr`. Prefix and postfix assignment operators are neat because they allow you to create operators such as pre-increment or pre-decrement.

Assign operators roughly translate as `lhs op val -> lhs = op(lhs, val)`.

Examples:

    // lazy infixr assignment
    assign lazy (??=) = function (a, b) {
      var val = a();
      return val !== undefined ? val : b();
    };

    // prefix assignment
    assign prefix (+++) = function (a) {
      return a + 2;
    };

## Precedence
Precedence can be declared for infix operators, though pre/postfix and assignment operators have a designated precedence. From lowest to highest precedence:

    1  ,
    2         // reserved for assignment operators; use assign declaration
    3  ||
    4  &&
    5  |
    6  ^
    7  &
    8  == != === !==
    9  < <= > >=
    10 >> >>> <<
    11 + -
    12 * % /
    14        // reserved for unary; use prefix declaration
    15        // reserved for postfix; use postfix declaration
    16 .

## Semantics

Operators adhere roughly to lexical scope -- they take affect from the point after they are declared to the point the script or function they were declared in ends. For an example of lexical scoping:

    (function () {
      assert.equal(1 + 2, 3);

      infixl 9 (+) = function (a, b) {
        return a * b;
      };

      assert.equal(1 + 2, 2);
    })();

    assert.equal(1 + 2, 3);

The user defined `+` operator only affects the code in the function it was declared in, after it is declared.

### deleteop

You can delete operators using the `deleteop` statement, e.g. `deleteop myop;`. Any further occurances of the operators symbol after that point will be interpreted as if the operator had not been deblared. It will delete the operator in the most recent scoped if operators are being shadowed.

### Refencing an Operator

You can gain a reference to an operator by using the `(op)` expression. This syntax is inspired by *sections* in Haskell, but without partial application support. Example:

    infixl * (^) = function (a, b) {
      return Math.pow(a, b);
    };
    var pow = (^);
    assert.equal(pow(2, 10), 1024);

### Other notes

* Operators are not restricted in what characters they can contain, besides whitespace. You can include a `)` by escaping it with `)`.
* You can define an operator using a reference to a function instead of a function expression. E.g.:

### Gotchas
* The lexer takes the longest possible match for a token, so if your operator is a valid part of another token, it should have space seperating it.
* No eval support.
