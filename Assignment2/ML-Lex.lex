structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n => (pos := (!pos) + 1; lex());
{ws}+ => (lex());
"NOT" => (Tokens.NOT(!pos,!pos));
"AND" => (Tokens.AND(!pos,!pos));
"OR" => (Tokens.OR(!pos,!pos));
"XOR"=> (Tokens.XOR(!pos,!pos));
"EQUALS" => (Tokens.EQUALS(!pos,!pos));
"IMPLIES" => (Tokens.IMPLIES(!pos,!pos));
";" => (Tokens.TERM(!pos,!pos));
"TRUE" => (Tokens.CONST(yytext,!pos,!pos));
"FALSE" => (Tokens.CONST(yytext,!pos,!pos));
"IF" => (Tokens.IF(!pos,!pos));
"THEN" => (Tokens.THEN(!pos,!pos));
"ELSE" => (Tokens.ELSE(!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
{alpha}+ => (Tokens.ID(yytext,!pos,!pos));

. => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());
