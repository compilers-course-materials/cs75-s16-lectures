%{
open Expr

%}

%token <int> NUM
%token <string> ID
%token END LBRACK RBRACK DEF FST SND ADD1 SUB1 LPAREN RPAREN LET IN EQUAL COMMA PLUS MINUS TIMES IF COLON ELSECOLON TRUE FALSE ISBOOL ISNUM EQEQ LESS GREATER PRINT EOF

%left PLUS MINUS TIMES GREATER LESS EQEQ



%type <Expr.program> program

%start program

%%

const :
  | NUM { ENumber($1) }
  | TRUE { EBool(true) }
  | FALSE { EBool(false) }

prim1 :
  | ADD1 { Add1 }
  | SUB1 { Sub1 }
  | PRINT { Print }
  | ISBOOL { IsBool }
  | ISNUM { IsNum }
  | FST { Fst }
  | SND { Snd }

binds :
  | ID EQUAL expr { [($1, $3)] }
  | ID EQUAL expr COMMA binds { ($1, $3)::$5 }

ids :
  | ID { [$1] }
  | ID COMMA ids { $1::$3 }

exprs :
  | expr { [$1] }
  | expr COMMA exprs { $1::$3 }

simple_expr :
  | prim1 LPAREN expr RPAREN { EPrim1($1, $3) }
  | LPAREN expr COMMA expr RPAREN { EPair($2, $4) }
  | LPAREN expr RPAREN { $2 }
  | ID LPAREN exprs RPAREN { EApp($1, $3) }
  | ID LPAREN RPAREN { EApp($1, []) }
  | const { $1 }
  | ID { EId($1) }

binop_expr :
  | binop_expr PLUS binop_expr { EPrim2(Plus, $1, $3) }
  | binop_expr MINUS binop_expr { EPrim2(Minus, $1, $3) }
  | binop_expr TIMES binop_expr { EPrim2(Times, $1, $3) }
  | binop_expr EQEQ binop_expr { EPrim2(Equal, $1, $3) }
  | binop_expr LESS binop_expr { EPrim2(Less, $1, $3) }
  | binop_expr GREATER binop_expr { EPrim2(Greater, $1, $3) }
  | simple_expr { $1 }

expr :
  | LET binds IN expr { ELet($2, $4) }
  | IF expr COLON expr ELSECOLON expr { EIf($2, $4, $6) }
  | binop_expr { $1 }

decl :
  | DEF ID LPAREN RPAREN COLON expr { DFun($2, [], $6) }
  | DEF ID LPAREN ids RPAREN COLON expr { DFun($2, $4, $7) }

decls :
  | decl { [$1] }
  | decl decls { $1::$2 }

program :
  | decls expr EOF { Program($1, $2) }
  | expr EOF { Program([], $1) }

%%

