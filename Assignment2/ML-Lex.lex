exception UnknownToken
structure Tokens= Tokens

  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val row = ref 1;
  val onfirst = ref 1; 
  val col = ref 1;
  fun printbrac() = if ((!onfirst) =1) then (print("["); onfirst:=0) else () ;
  val eof = fn () => (print ("EOF]\n"); row:= 1; col:=1; onfirst :=1 ; Tokens.EOF(!pos, !pos))
  val error = fn (e) => TextIO.output(TextIO.stdOut,"Unknown token:" ^ (Int.toString(!row)) ^ ":" ^ (Int.toString(!col)) ^":"^ e ^"\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))

%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n => (onfirst := 0 ; row := !row+1 ; col:=1 ;pos := (!pos) + 1; lex());
{ws} => (col:= !col+1; lex());
"NOT" => ( printbrac() ;  print("NOT \"NOT\", "); col:= !col+3; Tokens.NOT(!pos,!pos));
"AND" => ( printbrac() ;  print("AND \"AND\", "); col:= !col+3; Tokens.AND(!pos,!pos));
"OR" => ( printbrac() ;  print("OR \"OR\", "); col:= !col+2; Tokens.OR(!pos,!pos));
"XOR"=> ( printbrac() ;  print("XOR \"XOR\", "); col:= !col+3; Tokens.XOR(!pos,!pos));
"EQUALS" => ( printbrac() ;  print("EQUALS \"EQUALS\", "); col:= !col+6; Tokens.EQUALS(!pos,!pos));
"IMPLIES" => ( printbrac() ;  print("IMPLIES \"IMPLIES\", "); col:= !col+7; Tokens.IMPLIES(!pos,!pos));
";" => ( printbrac() ;  print("TERM \";\", "); col:= !col+1; Tokens.TERM(!pos,!pos));
"TRUE" => ( printbrac() ;  print("CONST \"TRUE\", "); col:= !col+4; Tokens.CONST(yytext,!pos,!pos));
"FALSE" => ( printbrac() ;  print("CONST \"FALSE\", "); col:= !col+5; Tokens.CONST(yytext,!pos,!pos));
"IF" => ( printbrac() ;  print("IF \"IF\", "); col:= !col+2; Tokens.IF(!pos,!pos));
"THEN" => (  printbrac() ; print("THEN \"THEN\", "); col:= !col+4; Tokens.THEN(!pos,!pos));
"ELSE" => ( printbrac() ;  print("ELSE \"ELSE\", "); col:= !col+4; Tokens.ELSE(!pos,!pos));
"("      => ( printbrac() ;  print("LPAREN \"(\", "); col:= !col+1; Tokens.LPAREN(!pos,!pos));
")"      => ( printbrac() ;  print("RPAREN \")\", "); col:= !col+1; Tokens.RPAREN(!pos,!pos));
{alpha}+ => ( printbrac() ;  print("ID \""^yytext^"\", "); col:= !col+ size(yytext); Tokens.ID(yytext,!pos,!pos));

. => ( print ("Unknown token:" ^ (Int.toString(!row)) ^ ":" ^ (Int.toString(!col)) ^":"^ yytext ^"\n"); onfirst:=1 ;col :=1; row :=1 ; raise UnknownToken ;  lex());
