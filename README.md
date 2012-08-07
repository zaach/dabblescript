# DabbleScript

Dabble with JavaScript syntax.

    // define an "is" operator
    infixl 8 (is) = function (a, b) {
      return a === b;
    };

    assert.ok("everything" is "everything");

## Install

    $ npm install dabble

Or clone from git and run `make`.

## Use

Translate dabble scripts:

    $ dabble exaple/es-next-operators.dabble

## Operator Declarations

    infixl [lazy] <precedence> (op) = expr;

    infixr [lazy] <precedence> (op) = expr;

    prefix [lazy] (op) = expr;

    postfix [lazy] (op) = expr;

    assign [infixr|prefix|postfix] (op) = expr;


Infix operators can be left or right associative.

Assign operators roughly translate as `lhs op val` to `lhs = op(lhs, val)`.

Lazy operators will have their arguments wrapped in thunks. Precedence determines the order of operations (detailed below), and can be a number from 1 - 16 or a builtin operator (to copy that operators precedence).

### Infix Precedence

From lowest to highest precedence:

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

