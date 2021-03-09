(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name Bool

%term
  ID of string | NOT | AND | OR | XOR | EQUALS | IMPLIES | TERM | CONST | IF | THEN | ELSE | RPAREN | LPAREN | EOF | BINOP

%nonterm formula | program | statement | expression | term | factor | condition | value

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right IF THEN ELSE 
(*
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
*)
  (* %nonassoc*)
%start program

%verbose

%%

program : statement () | ()
statement : formula TERM ()
formula : expression () | condition ()
condition : IF formula THEN formula ELSE formula ()
expression : term IMPLIES expression () | term IMPLIES condition () | term ()
term : term BINOP factor () | term BINOP condition () | factor ()
factor : NOT factor () | NOT condition () | value ()
value : LPAREN formula RPAREN () | CONST () | ID ()

(*
program : statement () | ()
statement : formula TERM ()
formula : expression () | IF formula THEN formula ELSE formula ()
expression : term IMPLIES formula () | term ()
term : term BINOP formula () | factor ()
factor : NOT formula () | value ()
value : LPAREN formula RPAREN () | CONST () | ID ()
*)