structure BoolLrVals = BoolLrValsFun(structure Token = LrParser.Token)
structure BoolLex = BoolLexFun(structure Tokens = BoolLrVals.Tokens);
structure BoolParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = BoolLrVals.ParserData
     	       structure Lex = BoolLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    BoolParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  BoolParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = BoolLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = BoolParser.Stream.get lexer
    in
        if BoolParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer