
var parser = require("./parser").parser,
    nodes = require("./nodes"),
    mozNodes = require("./moznodes"),
    stringify = require("./stringify").stringify,
    dynalexer = require("./lexer");

function JSParser (options) {
    // Create a parser constructor and an instance
    this.parser = new Parser(options||{});
    this.parser.lexer = dynalexer.init(this.parser.lexer);
    this.parser.lexer.dynamicAction = dynamicAction.bind(this.parser.lexer);
}

JSParser.prototype = {
    parse: function (source, options) {
        return this.parser.parse(source, options);
    }
};

var defaultBuilder = {};
var mozBuilder = {};

// Define AST nodes
nodes.defineNodes(defaultBuilder);

// Define Mozilla style AST nodes
nodes.defineNodes(mozBuilder);
mozNodes.defineNodes(mozBuilder);

function Parser (options) {
    this.yy.source = options.source||null;
    this.yy.startLine = options.line || 1;
    this.yy.noloc = options.loc === false;
    this.yy.builder = options.builder||defaultBuilder;
    this.yy.operators = {};
}

Parser.prototype = parser;

// allow yy.NodeType calls in parser
for (var con in defaultBuilder) {
    if (defaultBuilder.hasOwnProperty(con)) {
        parser.yy[con] = function (name){
            var context = this;
            return function (a,b,c,d,e,f,g,h) {
                    return context.builder[name](a,b,c,d,e,f,g,h);
                };
            }(con);
    }
}

// used named arguments to avoid arguments array
parser.yy.Node = function Node (type, a,b,c,d,e,f,g,h) {
    var buildName = type[0].toLowerCase()+type.slice(1);
    if (this.builder && this.builder[buildName]) {
        return this.builder[buildName](a,b,c,d,e,f,g,h);
    } else if (mozBuilder[buildName]) {
        return mozBuilder[buildName](a,b,c,d,e,f,g,h);
    } else {
        throw 'no such node type: '+type;
    }
};

parser.yy.locComb = function (start, end) {
    start.last_line = end.last_line;
    start.last_column = end.last_column;
    start.range = [start.range[0], end.range[1]];
    return start;
};

parser.yy.loc = function (loc) {
    if (this.noloc) return null;
    if ("length" in loc) loc = this.locComb(loc[0],loc[1]);

    var newLoc = { start:  { line: this.startLine+loc.first_line - 1,
                             column: loc.first_column },
                   end:    { line: this.startLine+loc.last_line - 1,
                             column: loc.last_column },
                   range:  loc.range
                 };

    if (this.source || this.builder !== defaultBuilder)
      newLoc.source = this.source;
    return newLoc;
};

// Handle parse errors and recover from ASI
parser.yy.parseError = function (err, hash) {
    // don't print error for missing semicolon
    if (!((!hash.expected || hash.expected.indexOf("';'") >= 0) && (hash.token === 'CLOSEBRACE' || parser.yy.lineBreak || parser.yy.lastLineBreak || hash.token === 1 || parser.yy.doWhile))) {
        throw new SyntaxError(err);
    }
};

parser.lexer.options.ranges = true;

// used to check if last match was a line break (for ; insertion)
var realLex = parser.lexer.lex;
parser.lexer.lex = function () {
    parser.yy.lastLineBreak = parser.yy.lineBreak;
    parser.yy.lineBreak = false;
    return realLex.call(this);
};

var realNext = parser.lexer.next;
parser.lexer.next = function () {
    var ret = realNext.call(this);
    if (ret === 'COMMENT' || ret === 'COMMENT_BLOCK') {
        if (this.yy.options.comment) {
            this.yy.comments.push({range: this.yylloc.range, type: types[ret], value: this.yytext});
        }
        return;
    }
    if (ret && ret !== 1 && ret !== 199) {
        if (this.yy.options.tokens) {
            var tokens = this.yy.tokens;
            var last = tokens[tokens.length-1];
            if (tokens.length && (last.value == '/' || last.value == '/=')) {
                tokens[tokens.length-1] = tokenObject(this, ret);
                var t = tokens[tokens.length-1];
                t.range[0] = last.range[0];
                t.value = last.value + t.value;
            } else {
                this.yy.tokens.push(tokenObject(this, ret));
            }
        }
    }
    return ret;
};

var types = {
  "NULLTOKEN": "Null",
  "THISTOKEN": "Keyword",
  "VAR": "Keyword",
  "IDENT": "Identifier",
  "NUMBER": "Numeric",
  "STRING": "String",
  "REGEXP_BODY": "RegularExpression",
  "COMMENT": "Line",
  "COMMENT_BLOCK": "Block",
  "TRUETOKEN": "Boolean",
  "FALSETOKEN": "Boolean"
};

// Punctuator tokens
'OPENBRACE CLOSEBRACE [ ] ( ) { } . ; : , PLUSEQUAL MINUSEQUAL MULTEQUAL MODEQUAL ANDEQUAL OREQUAL XOREQUAL LSHIFTEQUAL RSHIFTEQUAL URSHIFTEQUAL DIVEQUAL LE GE STREQ STRNEQ EQEQ NE AND OR PLUSPLUS MINUSMINUS URSHIFT LSHIFT + - * % < > & | ^ ! ~ ? / ='.split(' ').forEach(function (token) {
  types[token] = 'Punctuator';
});

// Keyword tokens
'BREAK CASE CONTINUE DEBUGGER DEFAULT DELETETOKEN DO ELSE FINALLY FOR FUNCTION IF INTOKEN INSTANCEOF NEW RETURN SWITCH TRY CATCH THROW TYPEOF VAR VOIDTOKEN WHILE WITH CLASS CONSTTOKEN LET ENUM EXPORT EXTENDS IMPORT SUPERTOKEN IMPLEMENTS INTERFACE PACKAGE PRIVATE PROTECTED PUBLIC STATIC YIELD THISTOKEN EVAL ARGUMENTS'.split(' ').forEach(function (token) {
  types[token] = 'Keyword';
});

function tokenObject (lexer, token) {
    var symbols = lexer.yy.parser.terminals_;
    return {
        "type":   types[symbols[token] || token],
        "value":  lexer.match,
        "range":  lexer.yylloc.range
    };
}

parser.yy.escapeString = function (s) {
  return s.replace(/\\\n/,'').replace(/\\([^xubfnvrt0\\])/g, '$1');
};

var oldParse = parser.parse;
parser.parse = function (source, options) {
    this.yy.lineBreak = false;
    this.yy.inRegex = false;
    this.yy.ASI = false;
    this.yy.tokens = [];
    this.yy.raw = [];
    this.yy.comments = [];
    this.yy.options = options || {};
    this.yy.opContext = [{}];
    return oldParse.call(this,source);
};

// precedence is assigned based on operators already in the grammar
var OPS = {
  1:      'COMMA',
  ',':    'COMMA',
  2:      'ASSIGN',
  '=':    'ASSIGN',
  '+=':   'ASSIGN',
  '-=':   'ASSIGN',
  '*=':   'ASSIGN',
  '/=':   'ASSIGN',
  '%=':   'ASSIGN',
  '|=':   'ASSIGN',
  '^=':   'ASSIGN',
  '&=':   'ASSIGN',
  '<<=':  'ASSIGN',
  '>>=':  'ASSIGN',
  '>>>=': 'ASSIGN',
  3:      'OR',
  '||':   'OR',
  4:      'AND',
  '&&':   'AND',
  5:      'BOR',
  '|':    'BOR',
  6:      'BXOR',
  '^':    'BXOR',
  7:      'BAND',
  '&':    'BAND',
  8:      'EQ',
  '==':   'EQ',
  '!=':   'EQ',
  '===':  'EQ',
  '!==':  'EQ',
  9:      'REL',
  '<':    'REL',
  '<=':   'REL',
  '>':    'REL',
  '>=':   'REL',
  10:      'SHIFT',
  '>>':   'SHIFT',
  '>>>':  'SHIFT',
  '<<':   'SHIFT',
  11:     'PLUS',
  '+':    'PLUS',
  '-':    'PLUS',
  12:     'MULT',
  '*':    'MULT',
  '%':    'MULT',
  '/':    'MULT',
  13:     'DOT',
  '.':    'DOT'
};

// called by lexer on a match of a dynamic op
function dynamicAction (op, index) {
    return this.yy.getop(op) && this.yy.getop(op).token;
}

parser.yy.pushScope = function (scope) {
    this.opContext.push(scope || {});
};

parser.yy.popScope = function () {
    this.opContext.pop();
};

parser.yy.getop = function (op) {
    for (var i = this.opContext.length-1; i >=0; i--) {
        if (op in this.opContext[i]) return this.opContext[i][op];
    }
};

parser.yy.getopInScope = function (op) {
    return this.opContext[this.opContext.length - 1][op];
};

parser.yy.setop = function (op, val) {
    return this.opContext[this.opContext.length - 1][op] = val;
};

parser.yy.delop = function (op) {
    for (var i = this.opContext.length-1; i >=0; i--) {
        if (op in this.opContext[i]) {
            delete this.opContext[i][op];
            return;
        }
    }
};

// initialize a new operator, or reasign operator precedence
// operators have a token, assoc. with the grammar,
// and a function used in the generated AST for the operation
parser.yy.setInfix = function (op, fn, prec, assoc) {
    if (!this.getopInScope(op)) {
        this.setop(op, {});
    }
    if (prec) this.getop(op).token = 'INFIX'+assoc+'_'+OPS[prec];
    if (fn) this.getop(op).fn = fn;
};

parser.yy.setPostPrefix = function (op, fn, post) {
    if (!this.getopInScope(op)) {
        this.setop(op, {});
    }
    this.getop(op).token = post ? 'POSTFIX_OP' : 'PREFIX_OP';
    if (fn) this.getop(op).fn = fn;
};

// called by lexer to return the appropriate token w.r.t. precedence/fixity
// by default (e.g. for `fun` ops) use "||" precedence
parser.yy.replaceToken = function (op) {
    return this.getop(op) && this.getop(op).token || 'INFIXL_OR';
};

// returns the associated function identifier for an operator
// called when building the AST
parser.yy.funForOp = function (op) {
    return this.getop(op) && this.getop(op).fn || op.substr(1,op.length-2);
};

exports.Reflect = {
    parse: function (src, options) {
        return new JSParser(options).parse(src, options);
    },
    stringify: stringify,
    compile: function (src, options) {
        return this.stringify(this.parse(src,options));
    }
};

exports.parse = exports.Reflect.parse;
exports.stringify = stringify;
exports.builder = defaultBuilder;
exports.mozBuilder = mozBuilder;

