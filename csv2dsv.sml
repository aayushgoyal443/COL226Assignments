(* Exceptions to be handled  *)

exception emptyInputFile;
exception ImproperDoubleQuotes;
(* exception  *)
exception UnevenFields of string;

(* Functions for updating Parameters *)

fun updatequotes(countquotes, c: string) =
  if (c= "\"") then countquotes+1
  else countquotes;

fun updatecurr(countcurr,c: string, countquotes, delim1:string) = 
  if (c = delim1) andalso (countquotes mod 2)=0 then countcurr+1
  else if (c = "\n") andalso (countquotes mod 2)=0 then 0
  else countcurr;

fun updatemain(countcurr,countmain, countquotes, c: string) = 
  if countmain = ~1 andalso (c = "\n") andalso (countquotes mod 2)=0 then countcurr+1
  else countmain;

fun updaterecord(countquotes,c: string,countrecord) = 
  if ( (c) = "\n") andalso (countquotes mod 2)=0 then countrecord+1
  else countrecord;

fun updatestarting (countquotes, c: string,delim1: string) = 
  if ((c) = "\n") andalso (countquotes mod 2)=0 then true
  else if (c = delim1) andalso (countquotes mod 2)=0 then true
  else false;

fun updateinquotes (starting, c: string , inquotes) = 
   if starting andalso c= "\"" then true
   else if starting then false
   else inquotes; 

fun updatec(ins) = 
  let val c = TextIO.input1 ins in
    case c of 
      NONE => ""
    | SOME(c) => str(c)
  end;

(* Other helper functions *)

fun checkeven(countcurr, countmain, countrecord) = 
  if countmain = ~1 then ()
  else if countmain = countcurr then ()
  else raise UnevenFields("Expected: "^Int.toString(countmain)^" fields, Present: "^Int.toString(countcurr)^" fields on Line "^Int.toString(countrecord)^"\n");

(* fun checkquotes() *)

fun beforeD(inquotes, outs) = 
  if inquotes then () 
  else TextIO.output(outs, "\"");

fun afterLF(ins,outs) = 
  case TextIO.lookahead(ins) of
    NONE => ()
  | SOME(c) => (TextIO.output1(outs,#"\""));
  
fun printing(starting,inquotes,countquotes,countcurr,countmain,countrecord,c: string,delim1,delim2,ins,outs) =

  if starting andalso c= "\"" then ()
  else if ((c) = "\n") andalso (countquotes mod 2)=0 then (
    beforeD(inquotes, outs);
    TextIO.output(outs,"\n");
    afterLF(ins,outs);
    checkeven(countcurr+1,countmain,countrecord+1)
  )
  else if (c = delim1) andalso (countquotes mod 2)=0 then (
    beforeD(inquotes, outs);
    TextIO.output(outs,delim2);
    TextIO.output(outs,"\"")
  )
  else TextIO.output(outs,c);

fun helper(countquotes: int, countcurr: int, countmain: int, countrecord: int, starting: bool, inquotes: bool, c: string, delim1: string, delim2:string,ins,outs) =
  if c= "" then ( TextIO.closeIn ins; TextIO.closeOut outs )
  else ( printing(starting,inquotes,countquotes,countcurr,countmain,countrecord,c,delim1,delim2,ins,outs); helper(updatequotes(countquotes,c),updatecurr(countcurr,c,countquotes,delim1),updatemain(countcurr,countmain,countquotes,c),updaterecord(countquotes,c,countrecord),updatestarting(countquotes,c,delim1),updateinquotes(starting,c,inquotes),updatec(ins),delim1,delim2,ins,outs) );


(* Final function for chaning the delimters *)

fun convertDelimiters(infilename,delim1,outfilename,delim2) = 
  let
    val ins = TextIO.openIn infilename
    val outs = TextIO.openOut outfilename
    
  in
    let
      val c = TextIO.inputN (ins,1)
    in
      if c= "" then ( raise emptyInputFile)
      else if c= "\"" then (TextIO.output(outs,"\"") ; helper(1,0,~1,0,false,true,updatec(ins),str(delim1),str(delim2),ins,outs))
      else ( TextIO.output(outs,"\""); TextIO.output(outs,c) ; helper(0,0,~1,0,false,false,updatec(ins),str(delim1),str(delim2),ins,outs) )
      (* else print c *)
    end
  end
  handle UnevenFields(s) => print(s);

(* For Conversion between CSV and TSV *)
fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename,#",",outfilename,#"\t");
fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename,#"\t",outfilename,#",");

(* csv2tsv("himym.csv","rahul.txt"); *)
convertDelimiters("himym.csv",#",","semi.txt",#";");
(* csv2tsv("empty.csv","rahul.txt"); *)
(* csv2tsv("himym_uneven.csv","rahul.txt"); *)
(* csv2tsv("gene_alias.csv","khetomp.txt"); *)
