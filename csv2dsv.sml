(* Making the convert Delimiters fucntion *)

fun convertDelimiters(infilename, delim1, outfilename, delim2) =






(* Making the convert EOL function *)

fun convertNewlines(infilename, newline1, outfilename, newline2) =






(* Making the rest of functions with the help of above fucntions *)

(* For Conversion between CSV and TSV *)
fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename,#",",outfilename,#"\t");
fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename,#"\t",outfilename,#",");

(* For conversions between Unix and DOS *)
fun unix2dos(infilename, outfilename) = convertNewlines(infilename,"\n",outfilename,"\r\n");
fun dos2unix(incilename, outfilename) = convertNewlines(infilename,"\r\n",outfilename,"\n");