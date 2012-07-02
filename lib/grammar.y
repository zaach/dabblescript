%start Program

%nonassoc IF_WITHOUT_ELSE
%nonassoc ELSE

%right INFIXR_COMMA
%left  INFIXL_COMMA ','
%right INFIXR_ASSIGN '=' PLUSEQUAL MINUSEQUAL MULTEQUAL DIVEQUAL LSHIFTEQUAL RSHIFTEQUAL URSHIFTEQUAL ANDEQUAL XOREQUAL OREQUAL MODEQUAL
%left  INFIXL_ASSIGN ':'
%right INFIXR_OR
%left  INFIXL_OR OR
%right INFIXR_AND
%left  INFIXL_AND AND
%right INFIXR_BOR
%left  INFIXL_BOR '|'
%right INFIXR_BXOR
%left  INFIXL_BXOR '^'
%right INFIXR_BAND
%left  INFIXL_BAND '&'
%right INFIXR_EQ
%left  INFIXL_EQ EQEQ NE STREQ STRNEQ
%right INFIXR_REL
%left  INFIXL_REL LE GE '<' '>' INSTANCEOF INTOKEN
%right INFIXR_SHIFT
%left  INFIXL_SHIFT LSHIFT RSHIFT URSHIFT
%right INFIXR_PLUS
%left  INFIXL_PLUS '+' '-'
%right INFIXR_MULT
%left  INFIXL_MULT '*' '/' '%'
%right INFIXR_DOT
%left  INFIXL_DOT '.' '['

%%

Pattern
    : OPENBRACE CLOSEBRACE
      { $$ = yy.Node('ObjectPattern', [], yy.loc([@1,@2])); }
    | OPENBRACE FieldList CLOSEBRACE
      { $$ = yy.Node('ObjectPattern', $FieldList, yy.loc([@1,@3])); }
    | OPENBRACE FieldList ',' CLOSEBRACE
      { $$ = yy.Node('ObjectPattern', $FieldList, yy.loc([@1,@4])); }
    | '[' ']'
      { $$ = yy.Node('ArrayPattern', [], yy.loc([@1,@2])); }
    /*| '[' '...' Element ']'*/
    | '[' Elision ']'
      { $$ = yy.Node('ArrayPattern', [,], yy.loc([@1,@3])); }
    /*| '[' Elision '...' Element ']'*/
    | '[' ArrayPatternList ']'
      { $$ = yy.Node('ArrayPattern', $2, yy.loc([@1,@3])); }
    /*| '[' ArrayPatternList '...' Element ']'*/
    | '[' ArrayPatternList ',' ElisionOpt ']'
      { $$ = yy.Node('ArrayPattern', $2.concat($4), yy.loc([@1,@5])); }
    /*| '[' ArrayPatternList ',' ElisionOpt '...' Element ']'*/
    ;

ArrayPatternList
    : Element
      { $$ = [$1]; }
    | Elision Element
      { $$ = $1; $$.push($2); }
    | ArrayPatternList ',' ElisionOpt Element
      { $$ = $1.concat($3); $$.push($4); }
    ;

FieldList
    : Field
      { $$ = [$1]; }
    | FieldList ',' Field
      { $$ = $1; $$.push($3); }
    ;

Field
    : IDENT
      { $$ = {key:yy.Node('Identifier', $1,yy.loc(@1)),value:yy.Node('Identifier', $1,yy.loc(@1)),kind: "init"}; }
    | IDENT ':' Element
      { yy.locComb(@$,@3);$$ = {key:yy.Node('Identifier', $1,yy.loc(@1)),value:$3,kind: "init"}; }
    | STRING ':' Element
      { yy.locComb(@$,@3);$$ = {key:yy.Node('Literal', parseString($1),yy.loc(@1)),value:$3,kind: "init"}; }
    | NUMBER ':' Element
      { yy.locComb(@$,@3);$$ = {key:yy.Node('Literal', parseNum($1),yy.loc(@1)),value:$3,kind: "init"}; }
    ;

Element
    : Pattern
    | IDENT
      { $$ = yy.Node('Identifier', $1,yy.loc(@1)) }
    ;

  /* break up keywords to workaround some LALR(1) limitations */
IdentifierName
    : IDENT
    | Keyword
    ;

Keyword
    : NULLTOKEN
    | TRUETOKEN
    | FALSETOKEN
    | BREAK
    | CASE
    | CATCH
    | CONSTTOKEN
    | CONTINUE
    | DEBUGGER
    | DEFAULT
    | DELETETOKEN
    | DO
    | ELSE
    | FINALLY
    | FOR
    | FUNCTION
    | IF
    | INTOKEN
    | INSTANCEOF
    | LET
    | NEW
    | RETURN
    | SWITCH
    | THIS
    | THROW
    | TRY
    | TYPEOF
    | VAR
    | VOIDTOKEN
    | WHILE
    | WITH
    ;

Literal
    : NULLTOKEN
      { $$ = yy.Node('Literal', null, yy.loc(@1), yytext); }
    | TRUETOKEN
      { $$ = yy.Node('Literal', true, yy.loc(@1), yytext); }
    | FALSETOKEN
      { $$ = yy.Node('Literal', false, yy.loc(@1), yytext); }
    | NUMBER
      { $$ = yy.Node('Literal', parseNum($1), yy.loc(@1), yytext); }
    | STRING
      { $$ = yy.Node('Literal', parseString($1), yy.loc(@1), yy.raw[yy.raw.length-1]);}
    | RegularExpressionLiteralBegin REGEXP_BODY
      {
        var full = $1+$2;
        var body = full.slice(1,full.lastIndexOf('/'));
        var flags = full.slice(full.lastIndexOf('/')+1);
        $$ = yy.Node('Literal', new RegExp(body, parseString(flags)), yy.loc(yy.locComb(@$,@2)), full);
      }
    ;

RegularExpressionLiteralBegin
    : '/'
      { yy.lexer.begin('regex'); /*yy.lexer.unput($1)*/; $$ = $1; }
    | DIVEQUAL
      { yy.lexer.begin('regex'); /*yy.lexer.unput($1)*/; $$ = $1; }
    ;

Property
    : IDENT /* object shorthand strawman */
      { $$ = yy.Node('Property', yy.Node('Identifier', $1,yy.loc(@1)),yy.Node('Identifier', $1,yy.loc(@1)),"init", yy.loc(@1)); }
    | IDENT ':' AssignmentExpr
      { yy.locComb(@$,@3);$$ = yy.Node('Property', yy.Node('Identifier', $1,yy.loc(@1)),$3,"init", yy.loc(@$)); }
    | Keyword ':' AssignmentExpr
      { yy.locComb(@$,@3);$$ = yy.Node('Property', yy.Node('Identifier', $1,yy.loc(@1)),$3,"init", yy.loc(@$)); }
    | STRING ':' AssignmentExpr
      { yy.locComb(@$,@3);$$ = yy.Node('Property', yy.Node('Literal', parseString($1),yy.loc(@1), JSON.stringify($1)),$3,"init", yy.loc(@$)); }
    | NUMBER ':' AssignmentExpr
      { yy.locComb(@$,@3);$$ = yy.Node('Property', yy.Node('Literal', parseNum($1),yy.loc(@1), String($1)),$3,"init", yy.loc(@$)); }
    | IDENT IdentifierName '(' ')' Block
      {
          if ($1 !== 'get' && $1 !== 'set') throw new Error('Parse error, invalid set/get.'); // TODO: use jison ABORT when supported
          @$ = yy.locComb(@1,@5);
          var fun = yy.Node('FunctionExpression',null,[],$Block, false, false, yy.loc(@5));
          $$ = yy.Node('Property', yy.Node('Identifier', $2,yy.loc(@2)),fun,$1, yy.loc(@$));
      }
    | IDENT IdentifierName '(' FormalParameterList ')' Block
      {
          @$ = yy.locComb(@1,@6);
          if ($1 !== 'get' && $1 !== 'set') throw new Error('Parse error, invalid set/get.'); // TODO: use jison ABORT when supported
          var fun = yy.Node('FunctionExpression',null,$FormalParameterList,$Block,false,false,yy.loc(@6));
          $$ = yy.Node('Property', yy.Node('Identifier', $2,yy.loc(@2)),fun,$1, yy.loc(@$));
      }
    | IDENT KeyLiteral '(' ')' Block
      {
          if ($1 !== 'get' && $1 !== 'set') throw new Error('Parse error, invalid set/get.'); // TODO: use jison ABORT when supported
          @$ = yy.locComb(@1,@5);
          var fun = yy.Node('FunctionExpression',null,[],$Block, false, false, yy.loc(@5));
          $$ = yy.Node('Property', $2,fun,$1,yy.loc(@$));
      }
    | IDENT KeyLiteral '(' FormalParameterList ')' Block
      {
          @$ = yy.locComb(@1,@6);
          if ($1 !== 'get' && $1 !== 'set') throw new Error('Parse error, invalid set/get.'); // TODO: use jison ABORT when supported
          var fun = yy.Node('FunctionExpression',null,$FormalParameterList,$Block,false,false,yy.loc(@6));
          $$ = yy.Node('Property', $2,fun,$1,yy.loc(@$));
      }
    ;

KeyLiteral
    : NUMBER
      { $$ = yy.Node('Literal', parseNum($1), yy.loc(@1), yytext); }
    | STRING
      { $$ = yy.Node('Literal', parseString($1), yy.loc(@1), yy.lexer.match); }
    ;

PropertyList
    : Property
      { $$ = [$1]; }
    | PropertyList ',' Property
      { $$ = $1; $$.push($3); }
    ;

PrimaryExpr
    : PrimaryExprNoBrace
    | OPENBRACE CLOSEBRACE
      { $$ = yy.Node('ObjectExpression',[],yy.loc([@$,@2])); }
    | OPENBRACE PropertyList CLOSEBRACE
      { $$ = yy.Node('ObjectExpression',$2,yy.loc([@$,@3])); }
    | OPENBRACE PropertyList ',' CLOSEBRACE
      { $$ = yy.Node('ObjectExpression',$2,yy.loc([@$,@4])); }
    ;

PrimaryExprNoBrace
    : THISTOKEN
      { $$ = yy.Node('ThisExpression', yy.loc(@1)); }
    | Literal
    | ArrayLiteral
    | IDENT
      { $$ = yy.Node('Identifier', String($1), yy.loc(@1)); }
    | '(' Expr ')'
      { $$ = $Expr; if($$.loc){$$.loc = yy.loc([@$,@3]); $$.range = $$.loc.range; delete $$.loc.range;} }
    ;

ArrayLiteral
    : '[' ']'
      { $$ = yy.Node('ArrayExpression',[],yy.loc([@$,@2])); }
    | '[' Elision ']'
      { $$ = yy.Node('ArrayExpression',$2,yy.loc([@$,@3])); }
    | '[' ElementList ']'
      { $$ = yy.Node('ArrayExpression',$2,yy.loc([@$,@3])); }
    | '[' ElementList ',' ElisionOpt ']'
      { $$ = yy.Node('ArrayExpression',$2.concat($4),yy.loc([@$,@5]));}
    ;

ElementList
    : AssignmentExpr
      { $$ = [$1]; }
    | Elision AssignmentExpr
      { $$ = $1; $$.push($2); }
    | ElementList ',' ElisionOpt AssignmentExpr
      { $$ = $1.concat($3); $$.push($4); }
    ;

ElisionOpt
    :
      { $$ = []; }
    | Elision
    ;

Elision
    : ','
      { $$ = [,]; }
    | Elision ','
      { $$ = $1; $$.length = $$.length+1; }
    ;

MemberExpr
    : PrimaryExpr
    | FunctionExpr
    | MemberExpr '[' Expr ']'
      { $$ = yy.Node('MemberExpression',$1,$3,true,yy.loc([@$,@4])); }
    | MemberExpr '.' IdentifierName
      { $$ = yy.Node('MemberExpression',$1,yy.Node('Identifier', String($3), yy.loc(@3)),false,yy.loc([@$,@3])); }
    | NEW MemberExpr Arguments
      { $$ = yy.Node('NewExpression',$MemberExpr,$Arguments,yy.loc([@$,@3])); }
    ;

MemberExprNoBF
    : PrimaryExprNoBrace
    | MemberExprNoBF '[' Expr ']'
      { $$ = yy.Node('MemberExpression',$1,$3,true,yy.loc([@$,@4])); }
    | MemberExprNoBF '.' IdentifierName
      { $$ = yy.Node('MemberExpression',$1,yy.Node('Identifier', String($3), yy.loc(@3)),false,yy.loc([@$,@3])); }
    | NEW MemberExpr Arguments
      { $$ = yy.Node('NewExpression',$MemberExpr,$Arguments,yy.loc([@$,@3])); }
    ;

NewExpr
    : MemberExpr
    | NEW NewExpr
      { $$ = yy.Node('NewExpression',$2,[],yy.loc([@$,@2])); }
    ;

NewExprNoBF
    : MemberExprNoBF
    | NEW NewExpr
      { $$ = yy.Node('NewExpression',$2,[],yy.loc([@$,@2])); }
    ;

CallExpr
    : MemberExpr Arguments
      { $$ = yy.Node('CallExpression',$1,$2,yy.loc([@$,@2])); }
    | CallExpr Arguments
      { $$ = yy.Node('CallExpression',$1,$2,yy.loc([@$,@2])); }
    | CallExpr '[' Expr ']'
      { $$ = yy.Node('MemberExpression',$1,$3,true,yy.loc([@$,@4])); }
    | CallExpr '.' IdentifierName
      { $$ = yy.Node('MemberExpression',$1,yy.Node('Identifier', String($3), yy.loc(@3)),false,yy.loc([@$,@3])); }
    ;

CallExprNoBF
    : MemberExprNoBF Arguments
      { $$ = yy.Node('CallExpression',$1,$2,yy.loc([@$,@2])); }
    | CallExprNoBF Arguments
      { $$ = yy.Node('CallExpression',$1,$2,yy.loc([@$,@2])); }
    | CallExprNoBF '[' Expr ']'
      { $$ = yy.Node('MemberExpression',$1,$3,true,yy.loc([@$,@4])); }
    | CallExprNoBF '.' IdentifierName
      { $$ = yy.Node('MemberExpression',$1,yy.Node('Identifier', String($3), yy.loc(@3)),false,yy.loc([@$,@3])); }
    ;

Arguments
    : '(' ')'
      { $$ = []; }
    | '(' ArgumentList ')'
      { $$ = $ArgumentList; }
    ;

ArgumentList
    : AssignmentExpr
      { $$ = [$1]; }
    | ArgumentList ',' AssignmentExpr
      { $$ = $1; $$.push($3); }
    ;

LeftHandSideExpr
    : NewExpr
    | CallExpr
    ;

LeftHandSideExprNoBF
    : NewExprNoBF
    | CallExprNoBF
    ;

PostfixExpr
    : LeftHandSideExpr
    | LeftHandSideExpr PLUSPLUS
      { $$ = yy.Node('UpdateExpression','++',$1,false,yy.loc([@$,@2])); }
    | LeftHandSideExpr MINUSMINUS
      { $$ = yy.Node('UpdateExpression','--',$1,false,yy.loc([@$,@2])); }
    ;

PostfixExprNoBF
    : LeftHandSideExprNoBF
    | LeftHandSideExprNoBF PLUSPLUS
      { $$ = yy.Node('UpdateExpression','++',$1,false,yy.loc([@$,@2])); }
    | LeftHandSideExprNoBF MINUSMINUS
      { $$ = yy.Node('UpdateExpression','--',$1,false,yy.loc([@$,@2])); }
    ;

UnaryExprCommon
    : DELETETOKEN UnaryExpr
      { $$ = yy.Node('UnaryExpression','delete',$2,true,yy.loc([@$,@2])); }
    | VOIDTOKEN UnaryExpr
      { $$ = yy.Node('UnaryExpression','void',$2,true,yy.loc([@$,@2])); }
    | TYPEOF UnaryExpr
      { $$ = yy.Node('UnaryExpression','typeof',$2,true,yy.loc([@$,@2])); }
    | PLUSPLUS UnaryExpr
      { $$ = yy.Node('UpdateExpression','++',$2,true,yy.loc([@$,@2])); }
    | MINUSMINUS UnaryExpr
      { $$ = yy.Node('UpdateExpression','--',$2,true,yy.loc([@$,@2])); }
    | '+' UnaryExpr
      { $$ = yy.Node('UnaryExpression','+',$2,true,yy.loc([@$,@2])); }
    | '-' UnaryExpr
      { $$ = yy.Node('UnaryExpression','-',$2,true,yy.loc([@$,@2])); }
    | '~' UnaryExpr
      { $$ = yy.Node('UnaryExpression','~',$2,true,yy.loc([@$,@2])); }
    | '!' UnaryExpr
      { $$ = yy.Node('UnaryExpression','!',$2,true,yy.loc([@$,@2])); }
    ;

UnaryExpr
    : PostfixExpr
    | UnaryExprCommon
    ;

UnaryExprNoBF
    : PostfixExprNoBF
    | UnaryExprCommon
    ;

MultiplicativeExpr
    : UnaryExpr
    | MultiplicativeExpr '*' UnaryExpr
      { $$ = yy.Node('BinaryExpression', '*', $1, $3, yy.loc([@$,@3])); }
    | MultiplicativeExpr '/' UnaryExpr
      { $$ = yy.Node('BinaryExpression', '/', $1, $3,yy.loc([@$,@3])); }
    | MultiplicativeExpr '%' UnaryExpr
      { $$ = yy.Node('BinaryExpression', '%', $1, $3,yy.loc([@$,@3])); }
    ;

MultiplicativeExprNoBF
    : UnaryExprNoBF
    | MultiplicativeExprNoBF '*' UnaryExpr
      { $$ = yy.Node('BinaryExpression',  '*', $1, $3,yy.loc([@$,@3])); }
    | MultiplicativeExprNoBF '/' UnaryExpr
      { $$ = yy.Node('BinaryExpression', '/', $1, $3,yy.loc([@$,@3])); }
    | MultiplicativeExprNoBF '%' UnaryExpr
      { $$ = yy.Node('BinaryExpression', '%', $1, $3,yy.loc([@$,@3])); }
    ;

AdditiveExpr
    : MultiplicativeExpr
    | AdditiveExpr '+' MultiplicativeExpr
      { $$ = yy.Node('BinaryExpression', '+', $1, $3,yy.loc([@$,@3])); }
    | AdditiveExpr '-' MultiplicativeExpr
      { $$ = yy.Node('BinaryExpression', '-', $1, $3,yy.loc([@$,@3])); }
    ;

AdditiveExprNoBF
    : MultiplicativeExprNoBF
    | AdditiveExprNoBF '+' MultiplicativeExpr
      { @$ = yy.locComb(@1,@3);
        $$ = yy.Node('BinaryExpression', '+', $1, $3, yy.loc(@$)); }
    | AdditiveExprNoBF '-' MultiplicativeExpr
      { @$ = yy.locComb(@1,@3);
        $$ = yy.Node('BinaryExpression', '-', $1, $3, yy.loc(@$)); }
    ;

ShiftExpr
    : AdditiveExpr
    | ShiftExpr LSHIFT AdditiveExpr
      { $$ = yy.Node('BinaryExpression', '<<', $1, $3,yy.loc([@$,@3])); }
    | ShiftExpr RSHIFT AdditiveExpr
      { $$ = yy.Node('BinaryExpression', '>>', $1, $3,yy.loc([@$,@3])); }
    | ShiftExpr URSHIFT AdditiveExpr
      { $$ = yy.Node('BinaryExpression', '>>>', $1, $3,yy.loc([@$,@3])); }
    ;

ShiftExprNoBF
    : AdditiveExprNoBF
    | ShiftExprNoBF LSHIFT AdditiveExpr
      { $$ = yy.Node('BinaryExpression', '<<', $1, $3,yy.loc([@$,@3])); }
    | ShiftExprNoBF RSHIFT AdditiveExpr
      { $$ = yy.Node('BinaryExpression', '>>', $1, $3,yy.loc([@$,@3])); }
    | ShiftExprNoBF URSHIFT AdditiveExpr
      { $$ = yy.Node('BinaryExpression', '>>>', $1, $3,yy.loc([@$,@3])); }
    ;

RelationalExpr
    : ShiftExpr
    | RelationalExpr '<' ShiftExpr
      { $$ = yy.Node('BinaryExpression', '<', $1, $3,yy.loc([@$,@3])); }
    | RelationalExpr '>' ShiftExpr
      { $$ = yy.Node('BinaryExpression', '>', $1, $3,yy.loc([@$,@3])); }
    | RelationalExpr LE ShiftExpr
      { $$ = yy.Node('BinaryExpression', '<=', $1, $3,yy.loc([@$,@3])); }
    | RelationalExpr GE ShiftExpr
      { $$ = yy.Node('BinaryExpression', '>=', $1, $3,yy.loc([@$,@3])); }
    | RelationalExpr INSTANCEOF ShiftExpr
      { $$ = yy.Node('BinaryExpression', 'instanceof', $1, $3,yy.loc([@$,@3])); }
    | RelationalExpr INTOKEN ShiftExpr
      { $$ = yy.Node('BinaryExpression', 'in', $1, $3,yy.loc([@$,@3])); }
    ;

RelationalExprNoIn
    : ShiftExpr
    | RelationalExprNoIn '<' ShiftExpr
      { $$ = yy.Node('BinaryExpression', '<', $1, $3,yy.loc([@$,@3])); }
    | RelationalExprNoIn '>' ShiftExpr
      { $$ = yy.Node('BinaryExpression', '>', $1, $3,yy.loc([@$,@3])); }
    | RelationalExprNoIn LE ShiftExpr
      { $$ = yy.Node('BinaryExpression', '<=', $1, $3,yy.loc([@$,@3])); }
    | RelationalExprNoIn GE ShiftExpr
      { $$ = yy.Node('BinaryExpression', '>=', $1, $3,yy.loc([@$,@3])); }
    | RelationalExprNoIn INSTANCEOF ShiftExpr
      { $$ = yy.Node('BinaryExpression', 'instanceof', $1, $3,yy.loc([@$,@3])); }
    ;

RelationalExprNoBF
    : ShiftExprNoBF
    | RelationalExprNoBF '<' ShiftExpr
      { $$ = yy.Node('BinaryExpression', '<', $1, $3,yy.loc([@$,@3])); }
    | RelationalExprNoBF '>' ShiftExpr
      { $$ = yy.Node('BinaryExpression', '>', $1, $3,yy.loc([@$,@3])); }
    | RelationalExprNoBF LE ShiftExpr
      { $$ = yy.Node('BinaryExpression', '<=', $1, $3,yy.loc([@$,@3])); }
    | RelationalExprNoBF GE ShiftExpr
      { $$ = yy.Node('BinaryExpression', '>=', $1, $3,yy.loc([@$,@3])); }
    | RelationalExprNoBF INSTANCEOF ShiftExpr
      { $$ = yy.Node('BinaryExpression', 'instanceof', $1, $3,yy.loc([@$,@3])); }
    | RelationalExprNoBF INTOKEN ShiftExpr
      { $$ = yy.Node('BinaryExpression', 'in', $1, $3,yy.loc([@$,@3])); }
    ;

EqualityExpr
    : RelationalExpr
    | EqualityExpr EQEQ RelationalExpr
      { $$ = yy.Node('BinaryExpression', '==', $1, $3,yy.loc([@$,@3])); }
    | EqualityExpr NE RelationalExpr
      { $$ = yy.Node('BinaryExpression', '!=', $1, $3,yy.loc([@$,@3])); }
    | EqualityExpr STREQ RelationalExpr
      { $$ = yy.Node('BinaryExpression', '===', $1, $3,yy.loc([@$,@3])); }
    | EqualityExpr STRNEQ RelationalExpr
      { $$ = yy.Node('BinaryExpression', '!==', $1, $3,yy.loc([@$,@3])); }
    ;

EqualityExprNoIn
    : RelationalExprNoIn
    | EqualityExprNoIn EQEQ RelationalExprNoIn
      { $$ = yy.Node('BinaryExpression', '==', $1, $3,yy.loc([@$,@3])); }
    | EqualityExprNoIn NE RelationalExprNoIn
      { $$ = yy.Node('BinaryExpression', '!=', $1, $3,yy.loc([@$,@3])); }
    | EqualityExprNoIn STREQ RelationalExprNoIn
      { $$ = yy.Node('BinaryExpression', '===', $1, $3,yy.loc([@$,@3])); }
    | EqualityExprNoIn STRNEQ RelationalExprNoIn
      { $$ = yy.Node('BinaryExpression', '!==', $1, $3,yy.loc([@$,@3])); }
    ;

EqualityExprNoBF
    : RelationalExprNoBF
    | EqualityExprNoBF EQEQ RelationalExpr
      { $$ = yy.Node('BinaryExpression', '==', $1, $3,yy.loc([@$,@3])); }
    | EqualityExprNoBF NE RelationalExpr
      { $$ = yy.Node('BinaryExpression', '!=', $1, $3,yy.loc([@$,@3])); }
    | EqualityExprNoBF STREQ RelationalExpr
      { $$ = yy.Node('BinaryExpression', '===', $1, $3,yy.loc([@$,@3])); }
    | EqualityExprNoBF STRNEQ RelationalExpr
      { $$ = yy.Node('BinaryExpression', '!==', $1, $3,yy.loc([@$,@3])); }
    ;

BitwiseANDExpr
    : EqualityExpr
    | BitwiseANDExpr '&' EqualityExpr
      { $$ = yy.Node('BinaryExpression', '&', $1, $3,yy.loc([@$,@3])); }
    ;

BitwiseANDExprNoIn
    : EqualityExprNoIn
    | BitwiseANDExprNoIn '&' EqualityExprNoIn
      { $$ = yy.Node('BinaryExpression', '&', $1, $3,yy.loc([@$,@3])); }
    ;

BitwiseANDExprNoBF
    : EqualityExprNoBF
    | BitwiseANDExprNoBF '&' EqualityExpr
      { $$ = yy.Node('BinaryExpression', '&', $1, $3,yy.loc([@$,@3])); }
    ;

BitwiseXORExpr
    : BitwiseANDExpr
    | BitwiseXORExpr '^' BitwiseANDExpr
      { $$ = yy.Node('BinaryExpression', '^', $1, $3,yy.loc([@$,@3])); }
    ;

BitwiseXORExprNoIn
    : BitwiseANDExprNoIn
    | BitwiseXORExprNoIn '^' BitwiseANDExprNoIn
      { $$ = yy.Node('BinaryExpression', '^', $1, $3,yy.loc([@$,@3])); }
    ;

BitwiseXORExprNoBF
    : BitwiseANDExprNoBF
    | BitwiseXORExprNoBF '^' BitwiseANDExpr
      { $$ = yy.Node('BinaryExpression', '^', $1, $3,yy.loc([@$,@3])); }
    ;

BitwiseORExpr
    : BitwiseXORExpr
    | BitwiseORExpr '|' BitwiseXORExpr
      { $$ = yy.Node('BinaryExpression', '|', $1, $3,yy.loc([@$,@3])); }
    ;

BitwiseORExprNoIn
    : BitwiseXORExprNoIn
    | BitwiseORExprNoIn '|' BitwiseXORExprNoIn
      { $$ = yy.Node('BinaryExpression', '|', $1, $3,yy.loc([@$,@3])); }
    ;

BitwiseORExprNoBF
    : BitwiseXORExprNoBF
    | BitwiseORExprNoBF '|' BitwiseXORExpr
      { $$ = yy.Node('BinaryExpression', '|', $1, $3,yy.loc([@$,@3])); }
    ;

LogicalANDExpr
    : BitwiseORExpr
    | LogicalANDExpr AND BitwiseORExpr
      { $$ = yy.Node('LogicalExpression', '&&', $1, $3,yy.loc([@$,@3])); }
    ;

LogicalANDExprNoIn
    : BitwiseORExprNoIn
    | LogicalANDExprNoIn AND BitwiseORExprNoIn
      { $$ = yy.Node('LogicalExpression', '&&', $1, $3,yy.loc([@$,@3])); }
    ;

LogicalANDExprNoBF
    : BitwiseORExprNoBF
    | LogicalANDExprNoBF AND BitwiseORExpr
      { $$ = yy.Node('LogicalExpression', '&&', $1, $3,yy.loc([@$,@3])); }
    ;

LogicalORExpr
    : LogicalANDExpr
    | LogicalORExpr OR LogicalANDExpr
      { $$ = yy.Node('LogicalExpression', '||', $1, $3,yy.loc([@$,@3])); }
    ;

LogicalORExprNoIn
    : LogicalANDExprNoIn
    | LogicalORExprNoIn OR LogicalANDExprNoIn
      { $$ = yy.Node('LogicalExpression', '||', $1, $3,yy.loc([@$,@3])); }
    ;

LogicalORExprNoBF
    : LogicalANDExprNoBF
    | LogicalORExprNoBF OR LogicalANDExpr
      { $$ = yy.Node('LogicalExpression', '||', $1, $3,yy.loc([@$,@3])); }
    ;

ConditionalExpr
    : LogicalORExpr
    | LogicalORExpr '?' AssignmentExpr ':' AssignmentExpr
      { $$ = yy.Node('ConditionalExpression', $1, $3, $5,yy.loc([@$,@5])); }
    ;

ConditionalExprNoIn
    : LogicalORExprNoIn
    | LogicalORExprNoIn '?' AssignmentExprNoIn ':' AssignmentExprNoIn
      { $$ = yy.Node('ConditionalExpression', $1, $3, $5,yy.loc([@$,@5])); }
    ;

ConditionalExprNoBF
    : LogicalORExprNoBF
    | LogicalORExprNoBF '?' AssignmentExpr ':' AssignmentExpr
      { $$ = yy.Node('ConditionalExpression', $1, $3, $5,yy.loc([@$,@5])); }
    ;

AssignmentExpr
    : ConditionalExpr %prec ':'
    | LeftHandSideExpr AssignmentOperator AssignmentExpr %prec '='
      { $$ = yy.Node('AssignmentExpression', $2, $1, $3,yy.loc([@$,@3])); }
    ;

AssignmentExprNoIn
    : ConditionalExprNoIn
    | LeftHandSideExpr AssignmentOperator AssignmentExprNoIn
      { $$ = yy.Node('AssignmentExpression', $2, $1, $3,yy.loc([@$,@3])); }
    ;

AssignmentExprNoBF
    : ConditionalExprNoBF
    | LeftHandSideExprNoBF AssignmentOperator AssignmentExpr
      { $$ = yy.Node('AssignmentExpression', $2, $1, $3,yy.loc([@$,@3])); }
    ;

AssignmentOperator
    : '='
    | PLUSEQUAL
    | MINUSEQUAL
    | MULTEQUAL
    | DIVEQUAL
    | LSHIFTEQUAL
    | RSHIFTEQUAL
    | URSHIFTEQUAL
    | ANDEQUAL
    | XOREQUAL
    | OREQUAL
    | MODEQUAL
    ;

Expr
    : AssignmentExpr
    | Expr ',' AssignmentExpr
      {
        if ($1.type == 'SequenceExpression') {
          $1.expressions.push($3);
          $1.loc = yy.loc([@$,@3]);
          $$ = $1;
        } else
          $$ = yy.Node('SequenceExpression',[$1, $3],yy.loc([@$,@3]));
      }
    ;

ExprNoIn
    : AssignmentExprNoIn
    | ExprNoIn ',' AssignmentExprNoIn
      {
        if ($1.type == 'SequenceExpression') {
          $1.expressions.push($3);
          $1.loc = yy.loc([@$,@3]);
          $$ = $1;
        } else
          $$ = yy.Node('SequenceExpression',[$1, $3],yy.loc([@$,@3]));
      }
    ;

ExprNoBF
    : AssignmentExprNoBF
    | ExprNoBF ',' AssignmentExpr
      {
        if ($1.type == 'SequenceExpression') {
          $1.expressions.push($3);
          $1.loc = yy.loc([@$,@3]);
          $$ = $1;
        } else
          $$ = yy.Node('SequenceExpression',[$1, $3],yy.loc([@$,@3]));
      }
    ;

Statement
    : Block
    | VariableStatement
    | FunctionDeclaration
    | EmptyStatement
    | ExprStatement
    | IfStatement
    | IterationStatement
    | ContinueStatement
    | BreakStatement
    | ReturnStatement
    | WithStatement
    | SwitchStatement
    | LabeledStatement
    | ThrowStatement
    | TryStatement
    | DebuggerStatement
    ;

Block
    : OPENBRACE CLOSEBRACE
      { $$ = yy.Node('BlockStatement',[],yy.loc([@$,@2])); }
    | OPENBRACE SourceElements CLOSEBRACE
      { $$ = yy.Node('BlockStatement',$2,yy.loc([@$,@3])); }
    ;

ConstStatement
    : CONSTTOKEN ConstDecralarionList ';'
      { $$ = yy.Node('VariableDeclaration', "const", $2, yy.loc([@$,@3])) }
    | CONSTTOKEN ConstDecralarionList error
      {
        if ($3.length) {
          @$.last_column = @3.first_column;
          @$.range[1] = @3.range[0];
        } else {
          yy.locComb(@$, @2);
        }

        $$ = yy.Node('VariableDeclaration', "const", $2, yy.loc(@$));
      }
    ;

ConstDecralarionList
    : IDENT
      { $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1,yy.loc(@1)), null, yy.loc(@1))]; }
    | IDENT Initializer
      { $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1,yy.loc(@1)), $2, yy.loc([@$, @2]))]; }
    | Pattern Initializer
      { $$ = [yy.Node('VariableDeclarator', $1, $2, yy.loc([@$, @2]))]; }
    | ConstDecralarionList ',' IDENT
      { yy.locComb(@$,@3);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3,yy.loc(@3)), null, yy.loc(@3))); }
    | ConstDecralarionList ',' IDENT Initializer
      { yy.locComb(@$,@4);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3,yy.loc(@3)), $4, yy.loc([@3, @4]))); }
    | ConstDecralarionList ',' Pattern Initializer
      { yy.locComb(@$,@4);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', $3, $4, yy.loc([@3, @4]))); }
    ;

ConstDecralarionListNoIn
    : IDENT
      { $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1,yy.loc(@1)), null, yy.loc(@$))]; }
    | IDENT InitializerNoIn
      { yy.locComb(@$,@2);
        $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1,yy.loc(@1)), $2, yy.loc([@$, @2]))]; }
    | Pattern InitializerNoIn
      { yy.locComb(@$,@2);$$ = [yy.Node('VariableDeclarator', $1, $2, yy.loc([@$, @2]))]; }
    | ConstDecralarionListNoIn ',' IDENT
      { yy.locComb(@$,@3);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3,yy.loc(@3)), null, yy.loc(@3))); }
    | ConstDecralarionListNoIn ',' IDENT InitializerNoIn
      { yy.locComb(@$,@4);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3,yy.loc(@3)), $4, yy.loc([@3, @4]))); }
    | ConstDecralarionListNoIn ',' Pattern InitializerNoIn
      { yy.locComb(@$,@4);$$ = $1; $1.push(yy.Node('VariableDeclarator', $3, $4, yy.loc([@3, @4]))); }
    ;

VariableStatement
    : VAR VariableDeclarationList ';'
      { $$ = yy.Node('VariableDeclaration', "var", $2, yy.loc([@$, @3])) }
    | VAR VariableDeclarationList error
      { errorLoc($3, @$, @2, @3);
        $$ = yy.Node('VariableDeclaration', "var", $2, yy.loc(@$)) }
    ;

VariableDeclarationList
    : IDENT
      { $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1,yy.loc(@1)), null, yy.loc(@1))]; }
    | IDENT Initializer
      { $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1,yy.loc(@1)), $2, yy.loc([@$, @2]))]; }
    | Pattern Initializer
      { $$ = [yy.Node('VariableDeclarator', $1, $2, yy.loc([@$, @2]))]; }
    | VariableDeclarationList ',' IDENT
      { yy.locComb(@$,@3);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3,yy.loc(@3)), null, yy.loc(@3))); }
    | VariableDeclarationList ',' IDENT Initializer
      { yy.locComb(@$,@4);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3,yy.loc(@3)), $4, yy.loc([@3, @4]))); }
    | VariableDeclarationList ',' Pattern Initializer
      { yy.locComb(@$,@4);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', $3, $4, yy.loc([@3, @4]))); }
    ;

VariableDeclarationListNoIn
    : IDENT
      { $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1,yy.loc(@1)), null, yy.loc(@$))]; }
    | IDENT InitializerNoIn
      { yy.locComb(@$,@2);
        $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1,yy.loc(@1)), $2, yy.loc([@$, @2]))]; }
    | Pattern InitializerNoIn
      { yy.locComb(@$,@2);$$ = [yy.Node('VariableDeclarator', $1, $2, yy.loc(@$))]; }
    | VariableDeclarationListNoIn ',' IDENT
      { yy.locComb(@$,@3);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3,yy.loc(@3)), null, yy.loc(@3))); }
    | VariableDeclarationListNoIn ',' IDENT InitializerNoIn
      { yy.locComb(@$,@4);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3,yy.loc(@3)), $4, yy.loc([@3, @4]))); }
    | VariableDeclarationListNoIn ',' Pattern InitializerNoIn
      { yy.locComb(@$,@4);$$ = $1; $1.push(yy.Node('VariableDeclarator', $3, $4, yy.loc([@3, @4]))); }
    ;

LetStatement
    : LET LetDeclarationList ';'
      { $$ = yy.Node('VariableDeclaration', "let", $2, yy.loc([@$,@3])) }
    | LET LetDeclarationList error
      {
        if ($3.length) {
          @$.last_column = @3.first_column;
          @$.range[1] = @3.range[0];
        } else {
          yy.locComb(@$, @2);
        }

        $$ = yy.Node('VariableDeclaration', "let", $2, yy.loc(@$));
      }
    ;

LetDeclarationList
    : IDENT
      { $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1,yy.loc(@1)), null, yy.loc(@1))]; }
    | IDENT Initializer
      { $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1,yy.loc(@1)), $2, yy.loc([@$, @2]))]; }
    | Pattern Initializer
      { $$ = [yy.Node('VariableDeclarator', $1, $2, yy.loc([@$, @2]))]; }
    | LetDeclarationList ',' IDENT
      { yy.locComb(@$,@3);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3,yy.loc(@3)), null, yy.loc(@3))); }
    | LetDeclarationList ',' IDENT Initializer
      { yy.locComb(@$,@4);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3,yy.loc(@3)), $4, yy.loc([@3, @4]))); }
    | LetDeclarationList ',' Pattern Initializer
      { yy.locComb(@$,@4);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', $3, $4, yy.loc([@3, @4]))); }
    ;

LetDeclarationListNoIn
    : IDENT
      { $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1, yy.loc(@1)), null, yy.loc(@$))]; }
    | IDENT InitializerNoIn
      { yy.locComb(@$,@2);
        $$ = [yy.Node('VariableDeclarator', yy.Node('Identifier', $1, yy.loc(@1)), $2, yy.loc([@$, @2]))]; }
    | Pattern InitializerNoIn
      { yy.locComb(@$,@2);$$ = [yy.Node('VariableDeclarator', $1, $2, yy.loc([@$, @2]))]; }
    | LetDeclarationListNoIn ',' IDENT
      { yy.locComb(@$, @3);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3, yy.loc(@3)), null, yy.loc(@3))); }
    | LetDeclarationListNoIn ',' IDENT InitializerNoIn
      { yy.locComb(@$, @4);
        $$ = $1; $1.push(yy.Node('VariableDeclarator', yy.Node('Identifier', $3, yy.loc(@3)), $4, yy.loc([@3, @4]))); }
    | LetDeclarationListNoIn ',' Pattern InitializerNoIn
      { yy.locComb(@$, @4);$$ = $1; $1.push(yy.Node('VariableDeclarator', $3, $4, yy.loc([@3, @4]))); }
    ;

Initializer
    : '=' AssignmentExpr
      { $$ = $2; yy.locComb(@$,@2) }
    ;

InitializerNoIn
    : '=' AssignmentExprNoIn
      { $$ = $2; yy.locComb(@$,@2) }
    ;

EmptyStatement
    : ';'
      { $$ = yy.Node('EmptyStatement',yy.loc(@1)); }
    ;

ExprStatement
    : ExprNoBF ';'
      { $$ = yy.Node('ExpressionStatement', $1,yy.loc([@$,@2])); }
    | ExprNoBF error
      {
        if (@1.last_line === @2.last_line) {
          if ($2.length) {
          @$.last_column = @2.first_column;
          @$.range[1] = @2.range[0];
          }else{
          @$.last_column = @2.last_column;
          @$.range[1] = @2.range[1];
          }
        } else {
          @$.last_column = @2.last_column;
          @$.last_line = @2.last_line;
          @$.range[1] = @2.range[1];
          /*console.log('!err', $1, @1);*/
          /*console.log('!err', $2, @2);*/
        }
        $$ = yy.Node('ExpressionStatement', $1, yy.loc(@$));
      }
    ;

IfStatement
    : IF '(' Expr ')' Statement %prec IF_WITHOUT_ELSE
      { $$ = yy.Node('IfStatement', $Expr, $Statement, null, yy.loc([@$,@5])); }
    | IF '(' Expr ')' Statement ELSE Statement
      { $$ = yy.Node('IfStatement', $Expr, $Statement1, $Statement2, yy.loc([@$,@7])); }
    ;

/* forces a reduction so the parser can be modified to allow ASI */
While
    : WHILE
      { $$ = $1; yy.doWhile = true; }
    ;

IterationStatement
    : DO Statement While '(' Expr ')' ';'
      { $$ = yy.Node('DoWhileStatement', $Statement, $Expr,yy.loc([@$,@7])); yy.doWhile = false; }
    | DO Statement While '(' Expr ')' error
      { $$ = yy.Node('DoWhileStatement', $Statement, $Expr,yy.loc([@$, @6])); yy.doWhile = false;}
    | WHILE '(' Expr ')' Statement
      { $$ = yy.Node('WhileStatement', $Expr, $Statement,yy.loc([@$,@5])); }
    | FOR '(' ExprNoInOpt ';' ExprOpt ';' ExprOpt ')' Statement
      { $$ = yy.Node('ForStatement', $ExprNoInOpt, $ExprOpt1, $ExprOpt2, $Statement,yy.loc([@$,@9])); }
    | FOR '(' VAR VariableDeclarationListNoIn ';' ExprOpt ';' ExprOpt ')' Statement
      { $$ = yy.Node('ForStatement',
                yy.Node('VariableDeclaration',"var", $4, yy.loc([@3,@4])),
                $ExprOpt1, $ExprOpt2, $Statement, yy.loc([@$,@10])); }
    | FOR '(' LET LetDeclarationListNoIn ';' ExprOpt ';' ExprOpt ')' Statement
      { $$ = yy.Node('ForStatement',
                yy.Node('VariableDeclaration',"let", $4, yy.loc([@3,@4])),
                $ExprOpt1, $ExprOpt2, $Statement, yy.loc([@$,@10])); }
    | FOR '(' CONSTTOKEN ConstDecralarionListNoIn ';' ExprOpt ';' ExprOpt ')' Statement
      { $$ = yy.Node('ForStatement',
                yy.Node('VariableDeclaration',"const", $4, yy.loc([@3,@4])),
                $ExprOpt1, $ExprOpt2, $Statement, yy.loc([@$,@10])); }
    | FOR '(' LeftHandSideExpr INTOKEN Expr ')' Statement
      { $$ = yy.Node('ForInStatement', $LeftHandSideExpr, $Expr, $Statement, false, yy.loc([@$,@7])); }
    | FOR '(' VarOrLet Expr ')' Statement
      { $$ = yy.Node('ForInStatement', $3,
                  $Expr, $Statement, false, yy.loc([@$,@6])); }
    | FOR '(' VarOrLetInitNoIn Expr ')' Statement
      { $$ = yy.Node('ForInStatement', $3,
                  $Expr, $Statement, false, yy.loc([@$,@6])); }
    ;

VarOrLet
    : VAR IDENT INTOKEN
      { $$ = yy.Node('VariableDeclaration',"var",
          [yy.Node('VariableDeclarator',yy.Node('Identifier', $2,yy.loc(@2)), null, yy.loc(@2))],
          yy.loc([@1,@2])) }
    | VAR Pattern INTOKEN
      { $$ = yy.Node('VariableDeclaration',"var",
          [yy.Node('VariableDeclarator',$2, null, yy.loc(@2))],
          yy.loc([@1,@2])) }
    | LET IDENT INTOKEN
      { $$ = yy.Node('VariableDeclaration',"let",
          [yy.Node('VariableDeclarator',yy.Node('Identifier', $2,yy.loc(@2)), null, yy.loc(@2))],
          yy.loc([@1,@2])) }
    | LET Pattern INTOKEN
      { $$ = yy.Node('VariableDeclaration',"let",
          [yy.Node('VariableDeclarator',$2, null, yy.loc(@2))],
          yy.loc([@1,@2])) }
    ;

VarOrLetInitNoIn
    : VAR IDENT InitializerNoIn INTOKEN
      { $$ = yy.Node('VariableDeclaration',"var",
          [yy.Node('VariableDeclarator',yy.Node('Identifier', $2,yy.loc(@2)), $3, yy.loc([@2, @3]))],
          yy.loc([@1,@3])) }
    | VAR Pattern InitializerNoIn INTOKEN
      { $$ = yy.Node('VariableDeclaration',"var",
          [yy.Node('VariableDeclarator',$2, $3, yy.loc([@2, @3]))],
          yy.loc([@1,@3])) }
    | LET IDENT InitializerNoIn INTOKEN
      { $$ = yy.Node('VariableDeclaration',"let",
          [yy.Node('VariableDeclarator',yy.Node('Identifier', $2,yy.loc(@2)), $3, yy.loc([@2, @3]))],
          yy.loc([@1,@3])) }
    | LET Pattern InitializerNoIn INTOKEN
      { $$ = yy.Node('VariableDeclaration',"let",
          [yy.Node('VariableDeclarator',$2, $3, yy.loc([@2, @3]))],
          yy.loc([@1,@3])) }
    ;

ExprOpt
    :
      { $$ = null }
    | Expr
    ;

ExprNoInOpt
    :
      { $$ = null }
    | ExprNoIn
    ;

ContinueStatement
    : CONTINUE ';'
      { $$ = yy.Node('ContinueStatement', null, yy.loc([@$, @2])); }
    | CONTINUE error
      { $$ = yy.Node('ContinueStatement', null, yy.loc([@$, ASIloc(@1)])); }
    | CONTINUE IDENT ';'
      { $$ = yy.Node('ContinueStatement', yy.Node('Identifier', $2, yy.loc(@2)), yy.loc([@$, @3])); }
    | CONTINUE IDENT error
      { errorLoc($3, @$, @2, @3);
        $$ = yy.Node('ContinueStatement', yy.Node('Identifier', $2, yy.loc(@2)), yy.loc(@$)); }
    ;

BreakStatement
    : BREAK ';'
      { $$ = yy.Node('BreakStatement', null, yy.loc([@$, @2])); }
    | BREAK error
      { $$ = yy.Node('BreakStatement', null, yy.loc([@$, ASIloc(@1)])); }
    | BREAK IDENT ';'
      { $$ = yy.Node('BreakStatement', yy.Node('Identifier', $2, yy.loc(@2)), yy.loc([@$, @3])); }
    | BREAK IDENT error
      { errorLoc($3, @$, @2, @3);
        $$ = yy.Node('BreakStatement', yy.Node('Identifier', $2, yy.loc(@2)), yy.loc(@$)); }
    ;

ReturnStatement
    : RETURN ';'
      { $$ = yy.Node('ReturnStatement', null, yy.loc([@$, @2])); }
    | RETURN error
      { $$ = yy.Node('ReturnStatement', null, yy.loc(ASIloc(@1))); }
    | RETURN Expr ';'
      { $$ = yy.Node('ReturnStatement', $2, yy.loc([@$, @3])); }
    | RETURN Expr error
      { $$ = yy.Node('ReturnStatement', $2, yy.loc([@$, ASIloc(@2)])); }
    ;

WithStatement
    : WITH '(' Expr ')' Statement
      { $$ = yy.Node('WithStatement', $Expr, $Statement, yy.loc([@$, @5])); }
    ;

SwitchStatement
    : SWITCH '(' Expr ')' CaseBlock
      { $$ = yy.Node('SwitchStatement', $Expr, $CaseBlock, false, yy.loc([@$, @5])); }
    ;

CaseBlock
    : OPENBRACE CaseClausesOpt CLOSEBRACE
      { $$ = $2; yy.locComb(@$,@3) }
    | OPENBRACE CaseClausesOpt DefaultClause CaseClausesOpt CLOSEBRACE
      { $2.push($DefaultClause); $$ = $2.concat($CaseClausesOpt2); yy.locComb(@$,@5) }
    ;

CaseClausesOpt
    :
      { $$ = []; }
    | CaseClauses
    ;

CaseClauses
    : CaseClause
      { $$ = [$1]; }
    | CaseClauses CaseClause
      { $1.push($2); $$ = $1; yy.locComb(@1, @2); }
    ;

CaseClause
    : CASE Expr ':'
      { $$ = yy.Node('SwitchCase',$Expr,[], yy.loc([@$,@3])); }
    | CASE Expr ':' SourceElements
      { $$ = yy.Node('SwitchCase',$Expr,$4, yy.loc([@$,@4])); }
    ;

DefaultClause
    : DEFAULT ':'
      { $$ = yy.Node('SwitchCase',null,[], yy.loc([@$,@2])); }
    | DEFAULT ':' SourceElements
      { $$ = yy.Node('SwitchCase',null,$3, yy.loc([@$,@3])); }
    ;

LabeledStatement
    : IDENT ':' Statement
      { $$ = yy.Node('LabeledStatement',yy.Node('Identifier', $1,yy.loc(@1)),$3, yy.loc([@$,@3])); }
    ;

ThrowStatement
    : THROW Expr ';'
      { $$ = yy.Node('ThrowStatement', $Expr, yy.loc([@$,@3])); }
    | THROW Expr error
      { errorLoc($3, @$, @2, @3);
        $$ = yy.Node('ThrowStatement', $Expr, yy.loc(@$)); }
    ;

TryStatement
    : TRY Block FINALLY Block
      { $$ = yy.Node('TryStatement', $Block1, null, $Block2, yy.loc([@$,@4])); }
    | TRY Block CATCH '(' IDENT ')' Block
      { $$ = yy.Node('TryStatement', $Block1,
                [yy.Node('CatchClause',yy.Node('Identifier', $5,yy.loc(@5)),null, $Block2, yy.loc([@3,@7]))], null, yy.loc([@$,@7])); }
    | TRY Block CATCH '(' IDENT ')' Block FINALLY Block
      { $$ = yy.Node('TryStatement', $Block1,
                [yy.Node('CatchClause',yy.Node('Identifier', $5,yy.loc(@5)),null, $Block2, yy.loc([@3,@7]))],
                $Block3, yy.loc([@$,@9])); }
    ;

DebuggerStatement
    : DEBUGGER ';'
      { $$ = yy.Node('DebuggerStatement', yy.loc([@$,@2])); }
    | DEBUGGER error
      { $$ = yy.Node('DebuggerStatement', yy.loc([@$, ASIloc(@1)])); }
    ;

FunctionDeclaration
    : FunctionScope IDENT '(' ')' Block
      { $$ = yy.Node('FunctionDeclaration',
                yy.Node('Identifier', $2,yy.loc(@2)), [], $Block, false, false, yy.loc([@$,@5]))
        leaveScope(yy);
      }
    | FunctionScope IDENT '(' FormalParameterList ')' Block
      { $$ = yy.Node('FunctionDeclaration',
                yy.Node('Identifier', $2,yy.loc(@2)),
                $FormalParameterList, $Block, false, false, yy.loc([@$,@6]))
        leaveScope(yy);
      }
    ;

FunctionExpr
    : FunctionScope '(' ')' Block
      { $$ = yy.Node('FunctionExpression', null, [], $Block, false, false, yy.loc([@$,@4]));
        leaveScope(yy);
      }
    | FunctionScope '(' FormalParameterList ')' Block
      { $$ = yy.Node('FunctionExpression', null,
           $FormalParameterList, $Block, false, false, yy.loc([@$,@5])); 
        leaveScope(yy);
      }

    | FunctionScope IDENT '(' ')' Block
      { $$ = yy.Node('FunctionExpression',
                yy.Node('Identifier', $2,yy.loc(@2)),
                [], $Block, false, false, yy.loc([@$,@5]));
        leaveScope(yy);
      }
    | FunctionScope IDENT '(' FormalParameterList ')' Block
      { $$ = yy.Node('FunctionExpression',
                yy.Node('Identifier', $2,yy.loc(@2)),
                $FormalParameterList, $Block, false, false, yy.loc([@$,@6]));
        leaveScope(yy);
      }
    ;

FunctionScope
    : FUNCTION
      { $$ = $1; enterScope(yy); }
    ;

FormalParameterList
    : IDENT
      { $$ = [yy.Node('Identifier', $1, yy.loc(@1))]; }
    | Pattern
      { $$ = [$1]; }
    | FormalParameterList ',' IDENT
      { $$ = $1; $$.push(yy.Node('Identifier', $3,yy.loc(@3))); yy.locComb(@$, @3); }
    | FormalParameterList ',' Pattern
      { $$ = $1; $$.push($3); yy.locComb(@$, @3); }
    ;

FunctionBody
    :
      { $$ = []; }
    | SourceElements
    ;

Program
    :
      {
        var prog = yy.Node('Program', [], {
            end: {column: 0, line: 0},
            start: {column: 0, line: 0},
        });
        prog.tokens = yy.tokens;
        prog.range = [0,0];
        return prog;
      }
    | SourceElements
      {
        var prog = yy.Node('Program',$1,yy.loc(@1));
        if (yy.tokens.length) prog.tokens = yy.tokens;
        if (yy.comments.length) prog.comments = yy.comments;
        if (prog.loc) prog.range = rangeBlock($1);
        return prog;
      }
    ;

SourceElements
    : SourceElement
      { $$ = [$1]; }
    | SourceElements SourceElement
      { yy.locComb(@$,@2);
        $$ = $1;$1.push($2); }
    ;

SourceElement
    : LetStatement
    | ConstStatement
    | Statement
    ;

/* infix rules */

SourceElements
    : InfixDeclaration
      { $$ = []; }
    | SourceElements InfixDeclaration
      { $$ = $1; }
    ;

SourceElement
    : OperatorDeclaration
    ;

MemberExpr
    : MemberExpr INFIXL_DOT PrimaryExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | MemberExpr INFIXL_DOT FunctionExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | PrimaryExpr INFIXR_DOT MemberExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | FunctionExpr INFIXR_DOT MemberExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

PostfixExpr
    : LeftHandSideExpr POSTFIX_OP
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1],yy.loc([@$,@2])); }
    ;

UnaryExprCommon
    : PREFIX_OP UnaryExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($1),yy.loc(@1)), [$2],yy.loc([@$,@2])); }
    ;

MultiplicativeExpr
    : MultiplicativeExpr INFIXL_MULT UnaryExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | UnaryExpr INFIXR_MULT MultiplicativeExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

AdditiveExpr
    : AdditiveExpr INFIXL_PLUS MultiplicativeExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | MultiplicativeExpr INFIXR_PLUS AdditiveExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

ShiftExpr
    : ShiftExpr INFIXL_SHIFT AdditiveExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | AdditiveExpr INFIXR_SHIFT ShiftExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

RelationalExpr
    : RelationalExpr INFIXL_REL ShiftExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | ShiftExpr INFIXR_REL RelationalExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

EqualityExpr
    : EqualityExpr INFIXL_EQ RelationalExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | RelationalExpr INFIXR_EQ EqualityExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

BitwiseANDExpr
    : BitwiseANDExpr INFIXL_BAND EqualityExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | EqualityExpr INFIXR_BAND BitwiseANDExpr
      { $$ = yy.Node('CallExpression',yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

BitwiseXORExpr
    : BitwiseXORExpr INFIXL_BXOR BitwiseANDExpr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | BitwiseANDExpr INFIXR_BXOR BitwiseXORExpr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

BitwiseORExpr
    : BitwiseORExpr INFIXL_BOR BitwiseXORExpr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | BitwiseXORExpr INFIXR_BOR BitwiseORExpr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

LogicalANDExpr
    : LogicalANDExpr INFIXL_AND BitwiseORExpr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | BitwiseORExpr INFIXR_AND LogicalANDExpr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

LogicalORExpr
    : LogicalORExpr INFIXL_OR LogicalANDExpr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | LogicalANDExpr INFIXR_OR LogicalORExpr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

AssignmentExpr
    : ConditionalExpr INFIXR_ASSIGN AssignmentExpr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | AssignmentExpr INFIXL_ASSIGN ConditionalExpr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

Expr
    : Expr INFIXL_COMMA AssignmentExpr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    | AssignmentExpr INFIXR_COMMA Expr
      { $$ = yy.Node('CallExpression', yy.Node('Identifier',yy.funForOp($2),yy.loc(@2)), [$1, $3],yy.loc([@$,@3])); }
    ;

InfixDeclaration
      /* e.g. infixl : * `elem`; */
    : INFIXL OpPrecedence INFIX_FN ';'
      { yy.setInfix($3, $3.substr(1,$3.length-2), $2, 'L'); }
    | INFIXR OpPrecedence INFIX_FN ';'
      { yy.setInfix($3, $3.substr(1,$3.length-2), $2, 'R'); }

      /* e.g. infixl : * (^); */
    | INFIXL OpPrecedence '(' OPERATOR ')' '=' IDENT ';'
      { yy.setInfix($4, $7, $2, 'L'); addOp(yy, $4); }
    | INFIXR OpPrecedence '(' OPERATOR ')' '=' IDENT ';'
      { yy.setInfix($4, $7, $2, 'R'); addOp(yy, $4); }

    | POSTFIX '(' OPERATOR ')' '=' IDENT ';'
      { yy.setPostPrefix($3, $6, true); addOp(yy, $3); }
    | PREFIX '(' OPERATOR ')' '=' IDENT ';'
      { yy.setPostPrefix($3, $6); addOp(yy, $3); }

    | DELETEOP OPERATOR ';'
      { deleteOp(yy, $2); }
    ;

OperatorDeclaration
      /* e.g. infixl : * (**) = exponentiation; */
    : INFIXL OpPrecedence '(' OPERATOR ')' '=' OperatorDefinition ';'
      {
        var funLabel = '$'+'infixop'+opLabels++;
        yy.setInfix($4, funLabel, $2, 'L');
        $$ = yy.Node('VariableDeclaration', "var",
                [yy.Node('VariableDeclarator', yy.Node('Identifier', funLabel, yy.loc(@1)), $OperatorDefinition)],
                yy.loc([@1,@8]));
        addOp(yy, $4);
      }
    | INFIXR OpPrecedence '(' OPERATOR ')' '=' OperatorDefinition ';'
      {
        var funLabel = '$'+'infixop'+opLabels++;
        yy.setInfix($4, funLabel, $2, 'R');
        $$ = yy.Node('VariableDeclaration', "var",
                [yy.Node('VariableDeclarator', yy.Node('Identifier', funLabel, yy.loc(@1)), $OperatorDefinition)],
                yy.loc([@1,@8]));
        addOp(yy, $4);
      }
    | POSTFIX '(' OPERATOR ')' '=' OperatorDefinition ';'
      {
        var funLabel = '$'+'postfixop'+opLabels++;
        yy.setPostPrefix($3, funLabel, true);
        $$ = yy.Node('VariableDeclaration', "var",
                [yy.Node('VariableDeclarator', yy.Node('Identifier', funLabel, yy.loc(@1)), $OperatorDefinition)],
                yy.loc([@1,@7]));
        addOp(yy, $3);
      }
    | PREFIX '(' OPERATOR ')' '=' OperatorDefinition ';'
      {
        var funLabel = '$'+'prefixop'+opLabels++;
        yy.setPostPrefix($3, funLabel);
        $$ = yy.Node('VariableDeclaration', "var",
                [yy.Node('VariableDeclarator', yy.Node('Identifier', funLabel, yy.loc(@1)), $OperatorDefinition)],
                yy.loc([@1,@7]));
        addOp(yy, $3);
      }
    ;

OperatorDefinition
    : FunctionOpt '(' ')' Block
      { $$ = yy.Node('FunctionExpression', null, [], $Block, false, false, yy.loc([@$,@4])); }
    | FunctionOpt '(' FormalParameterList ')' Block
      { $$ = yy.Node('FunctionExpression', null,
           $FormalParameterList, $Block, false, false, yy.loc([@$,@5])); }
    ;

FunctionOpt
    : FUNCTION
    |
    ;

OpPrecedence
    : InfixOp
    | NUMBER
    ;


InfixOp
    : ','
    | OR
    | AND
    | '|'
    | '^'
    | URSHIFT
    | LSHIFT
    | RSHIFT
    | '+'
    | '-'
    | '*'
    | '%'
    | '<'
    | '<<'
    | '<='
    | '>'
    | '>='
    | '>>'
    | '>>>'
    | '&'
    | '/'
    | '=='
    | '!='
    | '==='
    | '!=='
    | '.'
    | '='
    | PLUSEQUAL
    | MINUSEQUAL
    | MULTEQUAL
    | DIVEQUAL
    | MODEQUAL
    | OREQUAL
    | XOREQUAL
    | ANDEQUAL
    | LSHIFTEQUAL
    | RSHIFTEQUAL
    | URSHIFTEQUAL
    ;

%%

var opStack = [];
var opPointerStack = [];
var opPointer = 0;
var opLabels = 0;

// store the number of definied operators
function enterScope (yy) {
  opPointerStack.push(opPointer);
  yy.pushScope();
}

// pop operators defined since entering scope
function leaveScope (yy) {
  var previous = opPointerStack.pop();
  while (opPointer > previous) {
    deleteOpAt(yy, opPointer-1);
  }
  yy.popScope();
  /*console.error('leave', opStack, opPointer, opPointerStack);*/
}

// add op to lexer
function addOp (yy, op) {
  yy.lexer.addMatcher(opRegex(op), ['INITIAL']);
  opStack.push(op);
  /*console.error('add', op, opPointer, opStack);*/
  opPointer++;
}

function deleteOp (yy, op) {
  if (!yy.getop(op)) return;
  deleteOpAt(yy, opStack.lastIndexOf(op));
}

function deleteOpAt (yy, pointer) {
  // remove rule from lexer
  yy.lexer.removeMatcher(pointer);
  // delete entry in ops table
  yy.delop(opStack[pointer]);
  opStack.splice(pointer, 1);
  opPointer--;

  /*console.error('del', pointer, opStack, yy.operators, opPointer);*/
}

function opRegex (op) {
  return new RegExp('^'+op.replace(/([.?*+^$[\]\\(){}|-])/g, "\\$1"));
}

function rangeBlock (arr) {
    try {
      var ret = arr.length > 2 ? [arr[0].range[0], arr[arr.length-1].range[1]] : arr[0].range;
    } catch(e) {
      console.error('range error: '+e,'??', arr);
    }
    return ret;
}

function range (a, b) {
    return [a.range[0], b.range[1]];
}

function ASIloc (loc) {
    loc.last_column+=1;
    loc.range[1]=loc.range[1]+1;
    return loc;
}

function errorLoc (token, loc1, loc2, loc3) {
    if (token.length) {
      loc1.last_column = loc3.first_column;
      loc1.range[1] = loc3.range[0];
    } else {
      loc1.last_line = loc2.last_line;
      loc1.last_column = loc2.last_column;
      loc1.range = [loc1.range[0], loc2.range[1]];
    }
}

function parseNum (num) {
    if (num[0] === '0') {
        if (num[1] === 'x' || num[1] === 'X') {
            return parseInt(num, 16);
        }
        return parseInt(num, 8);
    } else {
        return Number(num);
    }
}

function parseString (str) {
    return str
              .replace(/\\(u[a-fA-F0-9]{4}|x[a-fA-F0-9]{2})/g, function (match, hex) {
                  return String.fromCharCode(parseInt(hex.slice(1), 16));
              })
              .replace(/\\([0-3]?[0-7]{1,2})/g, function (match, oct) {
                  return String.fromCharCode(parseInt(oct, 8));
              })
              .replace(/\\0[^0-9]?/g,'\u0000')
              .replace(/\\(?:\r\n?|\n)/g,'')
              .replace(/\\n/g,'\n')
              .replace(/\\r/g,'\r')
              .replace(/\\t/g,'\t')
              .replace(/\\v/g,'\v')
              .replace(/\\f/g,'\f')
              .replace(/\\b/g,'\b')
              .replace(/\\(.)/g, "$1");
}

