structure BoolLrVals = BoolLrValsFun(structure Token = LrParser.Token)
structure BoolLex = BoolLexFun(structure Tokens = BoolLrVals.Tokens);
structure BoolParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = BoolLrVals.ParserData
     	       structure Lex = BoolLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,col:int) =
		    	TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString pos) ^ ":" ^ (Int.toString col)^ s ^ "\n")
		in
		     BoolParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer infilename =
	let val ins = TextIO.openIn infilename in let val str:string = TextIO.inputAll(ins) in
    let val done = ref false
    	val lexer=  BoolParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end end end 
		
fun parse (lexer) =
    let val dummyEOF = BoolLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = BoolParser.Stream.get lexer
    in
        ( if BoolParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result ); print("EOF\n"))
    end

fun parseString (infilename) = parse (stringToLexer infilename) handle UnknownToken => ();