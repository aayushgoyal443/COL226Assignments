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

start : program ()
program : statement program () | ()
statement : formula TERM ()
formula : IF formula THEN formula ELSE formula ()
        |   formula IMPLIES formula ()
        |   formula AND formula () | formula OR formula () |   formula XOR formula () |   formula EQUALS formula ()
        |   NOT formula ()
        |   LPAREN formula RPAREN ()
        |   CONST ()
        |   ID ()