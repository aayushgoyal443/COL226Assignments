(* Making the convert Delimiters fucntion *)

fun changechar(count:int, c:char, delim1, delim2) =
  if (c = delim1) andalso (count mod 2)=0 then delim2
  else c;

fun update(count:int, c:char) =
  if (c= #"\"") then count+1
  else count;

fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
  let
    val ins = TextIO.openIn infilename
    val outs = TextIO.openOut outfilename
    fun helper(count,copt: char option) =
      case copt of
           NONE => (TextIO.closeIn ins; TextIO.closeOut outs)
        |  SOME(c) => (TextIO.output1(outs,changechar(count,c,delim1,delim2)); helper(update(count,c),TextIO.input1 ins));
  in
    helper(0,TextIO.input1 ins)
  end;


(* Making the rest of functions with the help of above fucntion *)

(* For Conversion between CSV and TSV *)
fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename,#",",outfilename,#"\t");
fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename,#"\t",outfilename,#",");