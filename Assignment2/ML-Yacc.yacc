(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name Bool

%term
  ID of string | NOT | AND | OR | XOR | EQUALS | IMPLIES | TERM | CONST | IF | THEN | ELSE | RPAREN | LPAREN | EOF

%nonterm formula | program | statement | expression | term | factor | condition | value | start | binop

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%start start

%verbose

%%

start : program ()
program : statement program () | ()
statement : formula TERM ()
formula : expression () | condition ()
condition : IF formula THEN formula ELSE formula ()
expression : term IMPLIES expression () | term IMPLIES condition () | term ()
term : term binop factor () | term binop condition () | factor ()
binop : AND () | OR () | XOR () | EQUALS ()
factor : NOT factor () | NOT condition () | value ()
value : LPAREN formula RPAREN () | CONST () | ID ()