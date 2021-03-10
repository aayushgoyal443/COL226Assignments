structure Tokens= Tokens

  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val row = ref 1;
  val firstline = ref 1; 
  val col = ref 1;
  val eof = fn () => (print ("EOF]\n"); row:= 1; col:=1; Tokens.EOF(!pos, !pos))
  val error = fn (e) => TextIO.output(TextIO.stdOut,"Unknown token:" ^ (Int.toString(!row)) ^ ":" ^ (Int.toString(!col)) ^":"^ e ^"\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))

%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n => (firstline := 0 ; row := !row+1 ; col:=1 ;pos := (!pos) + 1; lex());
{ws} => (col:= !col+1; lex());
"NOT" => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("NOT \"NOT\", "); col:= !col+3; Tokens.NOT(!pos,!pos));
"AND" => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("AND \"AND\", "); col:= !col+3; Tokens.AND(!pos,!pos));
"OR" => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("OR \"OR\", "); col:= !col+2; Tokens.OR(!pos,!pos));
"XOR"=> ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("XOR \"XOR\", "); col:= !col+3; Tokens.XOR(!pos,!pos));
"EQUALS" => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("EQUALS \"EQUALS\", "); col:= !col+6; Tokens.EQUALS(!pos,!pos));
"IMPLIES" => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("IMPLIES \"IMPLIES\", "); col:= !col+7; Tokens.IMPLIES(!pos,!pos));
";" => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("TERM \";\", "); col:= !col+1; Tokens.TERM(!pos,!pos));
"TRUE" => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("CONST \"TRUE\", "); col:= !col+4; Tokens.CONST(yytext,!pos,!pos));
"FALSE" => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("CONST \"FALSE\", "); col:= !col+5; Tokens.CONST(yytext,!pos,!pos));
"IF" => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("IF \"IF\", "); col:= !col+2; Tokens.IF(!pos,!pos));
"THEN" => (  if ((!firstline) =1) then (print("["); firstline:=0) else (); print("THEN \"THEN\", "); col:= !col+4; Tokens.THEN(!pos,!pos));
"ELSE" => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("ELSE \"ELSE\", "); col:= !col+4; Tokens.ELSE(!pos,!pos));
"("      => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("LPAREN \"(\", "); col:= !col+1; Tokens.LPAREN(!pos,!pos));
")"      => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("RPAREN \")\", "); col:= !col+1; Tokens.RPAREN(!pos,!pos));
{alpha}+ => ( if ((!firstline) =1) then (print("["); firstline:=0) else ();  print("ID \""^yytext^"\", "); col:= !col+ size(yytext); Tokens.ID(yytext,!pos,!pos));

. => (error (yytext) ; col := !col+1 ; lex());
