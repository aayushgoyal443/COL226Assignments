(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name Bool

%term
  ID of string | NOT | AND | OR | XOR | EQUALS | IMPLIES | TERM | CONST of string | IF | THEN | ELSE | RPAREN | LPAREN | EOF

%nonterm formula | program | statement | start

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right IF THEN ELSE 
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT

  (* %nonassoc*)
%start start

%verbose

%%

start : program (print("start -> program, "))
program : statement program (print("program -> statement program, ")) | ()
statement : formula TERM (print("statement -> formula, "))
formula : IF formula THEN formula ELSE formula (print("formula -> IF formula THEN formula ELSE formula, "))
        |   formula IMPLIES formula (print("formula -> formula IMPLIES formula, "))
        |   formula AND formula (print("formula -> formula AND formula, ")) | formula OR formula (print("formula -> formula OR formula, ")) |   formula XOR formula (print("formula -> formula XOR formula, ")) |   formula EQUALS formula (print("formula -> formula EQUALS formula, "))
        |   NOT formula (print("formula -> NOT formula, "))
        |   LPAREN formula RPAREN (print("formula -> LPAREN formula RPAREN, "))
        |   CONST (print("formula -> CONST, "))
        |   ID (print("formula -> ID, "))