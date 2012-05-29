
var parser = require("./parser").parser,
    nodes = require("./nodes"),
    stringify = require("./stringify").stringify,
    dynalexer = require("./lexer");

function JSParser (options) {
    // Create a parser constructor and an instance
    this.parser = new Parser(options||{});
    this.parser.lexer = dynalexer.init(this.parser.lexer);
    this.parser.lexer.dynamicAction = dynamicAction.bind(this.parser.lexer);
}

JSParser.prototype = {
    parse: function (source) {
        return this.parser.parse(source);
    }
};

var builder = {};

// Define AST nodes
nodes.defineNodes(builder);

function Parser (options) {
    this.yy.source = options.source||null;
    this.yy.startLine = options.line || 1;
    this.yy.noloc = options.loc === false;
    this.yy.builder = options.builder||null;
    this.yy.operators = {};
}

Parser.prototype = parser;

// allow yy.NodeType calls in parser
for (var con in builder) {
    if (builder.hasOwnProperty(con)) {
        parser.yy[con] = function (name){
            return function (a,b,c,d,e,f,g,h) {
                    return builder[name](a,b,c,d,e,f,g,h);
                };
            }(con);
    }
}

// used named arguments to avoid arguments array
parser.yy.Node = function Node (type, a,b,c,d,e,f,g,h) {
    var buildName = type[0].toLowerCase()+type.slice(1);
    if (this.builder && this.builder[buildName]) {
        return this.builder[buildName](a,b,c,d,e,f,g,h);
    } else if (builder[buildName]) {
        return builder[buildName](a,b,c,d,e,f,g,h);
    } else {
        throw 'no such node type: '+type;
    }
};

parser.yy.locComb = function (start, end) {
    start.last_line = end.last_line;
    start.last_column = end.last_column;
    return start;
};

parser.yy.loc = function (loc) {
    if (this.noloc) return null;
    if ("length" in loc) loc = this.locComb(loc[0],loc[1]);

    return { source: this.source,
             start:  { line: this.startLine+loc.first_line - 1,
                       column: loc.first_column },
             end:    { line: this.startLine+loc.last_line - 1,
                       column: loc.last_column }
           };
};

// Handle parse errors and recover from ASI
parser.yy.parseError = function (err, hash) {
    // don't print error for missing semicolon
    if (!(hash.expected.indexOf("';'") >= 0 && (hash.token === 'CLOSEBRACE' || parser.yy.lineBreak || parser.yy.lastLineBreak || hash.token === 1))) {
        throw new SyntaxError(err);
    }
};

// used to check if last match was a line break (for ; insertion)
var realLex = parser.lexer.lex;
parser.lexer.lex = function () {
    parser.yy.lastLineBreak = parser.yy.lineBreak;
    parser.yy.lineBreak = false;
    return realLex.call(parser.lexer);
};

parser.yy.escapeString = function (s) {
  return s.replace(/\\\n/,'').replace(/\\([^xubfnvrt0\\])/g, '$1');
};

var oldParse = parser.parse;
parser.parse = function (source) {
    parser.yy.lineBreak = false;
    parser.yy.inRegex = false;
    parser.yy.ASI = false;
    return oldParse.call(this,source);
};

// precedence is assigned based on operators already in the grammar
var OPS = {
  1:      'COMMA',
  ',':    'COMMA',
  2:      'OR',
  '||':   'OR',
  3:      'AND',
  '&&':   'AND',
  4:      'BOR',
  '|':    'BOR',
  5:      'BXOR',
  '^':    'BXOR',
  6:      'BAND',
  '&':    'BAND',
  7:      'EQ',
  '==':   'EQ',
  '!=':   'EQ',
  '===':  'EQ',
  '!==':  'EQ',
  8:      'REL',
  '<':    'REL',
  '<=':   'REL',
  '>':    'REL',
  '>=':   'REL',
  9:      'SHIFT',
  '>>':   'SHIFT',
  '>>>':  'SHIFT',
  '<<':   'SHIFT',
  10:     'PLUS',
  '+':    'PLUS',
  '-':    'PLUS',
  11:     'MULT',
  '*':    'MULT',
  '%':    'MULT',
  '/':    'MULT',
  12:     'DOT',
  '.':    'DOT'
};

// called by lexer on a match of a dynamic op
function dynamicAction (op, index) {
  return this.yy.operators[op] && this.yy.operators[op].token;
}

// initialize a new operator, or reasign operator precedence
// operators have a token, assoc. with the grammar,
// and a function used in the generated AST for the operation
parser.yy.setInfix = function (op, fn, prec, assoc) {
    if (!this.operators[op]) {
        this.operators[op] = {};
    }
    if (prec) this.operators[op].token = 'INFIX'+assoc+'_'+OPS[prec];
    if (fn) this.operators[op].fn = fn;
};

// called by lexer to return the appropriate token w.r.t. precedence/fixity
// by default (e.g. for `fun` ops) use "||" precedence
parser.yy.replaceToken = function (op) {
    return this.operators[op] && this.operators[op].token || 'INFIXL_OR';
};

// returns the associated function identifier for an operator
// called when building the AST
parser.yy.funForOp = function (op) {
    return this.operators[op] && this.operators[op].fn || op.substr(1,op.length-2);
};

exports.Reflect = {
    parse: function (src, options) {
        return new JSParser(options).parse(src);
    },
    stringify: stringify,
    compile: function (src, options) {
        return this.stringify(this.parse(src,options));
    }
};

exports.parse = exports.Reflect.parse;
exports.stringify = stringify;
exports.builder = builder;

