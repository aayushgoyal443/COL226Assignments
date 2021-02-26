(* Exceptions to be handled  *)

(* The input file exists but has no entry in it  *)
exception emptyInputFile; 
(* The double quotes have not been used in the desired format *)
exception ImproperDoubleQuotes;
(* After all the records are finished we still need ot give a line feed, if is absent then this exception is  *)
exception NoNewlineAtEOF
(* The number of fields in each record are not same  *)
exception UnevenFields of string;

(*
Parameters used in the functions below and their meaning :- 

countquotes: int :- counts the number of quotes we have parsed in input file (sml doesn't automatically escape a double quotes followed by a double quote)
countcurr: int :- counts the number of fields we have already parseed in the current record 
countmain: int :- Count of the number of fields in the first record. 
countrecord: int :- keeps the count of how many record we have parsed already. 
starting: bool :- this tells whether we are starting to tarverse a field or not, because accordingly some decisions need to be made 
inquotes: bool :- this keeps a record of whether the field in which we are moving is was already in double quotes in the input file  
c: string :- the current character (in string form)
delim1: string :- the delimeter through which the input file is already separated. It is changed in stirng to avoid unnecessary tycoon mismatch
delim2: string :- the delimeter in which the given file is to be converted. 
ins  :- input stream 
outs :- output stream
next :- it is used when we want to look the character just next to current character, it actually doesn't move the pointer.

*)


(* Functions for updating Parameters, each is of O(1) space and time complexity*)

(* this function updates the count of total number of double quotes *)
fun updatequotes(countquotes, c: string) =
  if (c= "\"") then countquotes+1
  else countquotes;

(* keeps a track of the number of fields in a record *)
fun updatecurr(countcurr,c: string, countquotes, delim1:string) = 
  if (c = delim1) andalso (countquotes mod 2)=0 then countcurr+1
  else if (c = "\n") andalso (countquotes mod 2)=0 then 0
  else countcurr;

(* To know the expected number of fields  *)
fun updatemain(countcurr,countmain, countquotes, c: string) = 
  if countmain = ~1 andalso (c = "\n") andalso (countquotes mod 2)=0 then countcurr+1
  else countmain;

(* to update the total number of records we have parsed *)
fun updaterecord(countquotes,c: string,countrecord) = 
  if ( (c) = "\n") andalso (countquotes mod 2)=0 then countrecord+1
  else countrecord;

(* to know whether we will be starting a field or not *)
fun updatestarting (countquotes, c: string,delim1: string) = 
  if ((c) = "\n") andalso (countquotes mod 2)=0 then true
  else if (c = delim1) andalso (countquotes mod 2)=0 then true
  else false;

(* to know whether the next field is in quotes or not  *)
fun updateinquotes (countquotes, c: string , inquotes,delim1,ins) = 
  let 
    val copt= TextIO.lookahead(ins);
  in 
    let val next = case copt of NONE => "" | SOME(copt) => str(copt) in
   if c = "\n" andalso (countquotes mod 2)=0 andalso next = "\"" then true
   else if c =delim1 andalso (countquotes mod 2)=0 andalso next = "\"" then true
   else if c = "\n" andalso (countquotes mod 2)=0 then false
   else if c = delim1 andalso (countquotes mod 2)=0 then false 
   else inquotes
  end end; 

(* to keep getting the next value of character in the input file, in the form of a string  *)
fun updatec(ins) = 
  let val c = TextIO.input1 ins in
    case c of 
      NONE => ""
    | SOME(c) => str(c)
  end;

(* Other helper functions *)

(* this checks whether the last entry record is finished by a line feed or not  *)
fun checkEOF(countquotes,ins,c :string) = 
  let val copt = TextIO.lookahead(ins) in 
    if (copt = NONE) andalso (c = "\n") andalso countquotes mod 2 =0 then ()
    else if (copt = NONE) then raise NoNewlineAtEOF
    else ()
  end;

(* (this covers whether the double quotes are in the proper format ) *)
fun checkescapedquotes(ins,c:string, inquotes, delim1, countquotes) = 
  let val copt  = TextIO.lookahead(ins) in 
    let val next = case copt of NONE => "" | SOME(copt) => str(copt) in 
      if (c= "\"") andalso countquotes mod 2 =0 andalso inquotes  then ()
      else if (c= "\"") andalso countquotes mod 2 =0 then raise ImproperDoubleQuotes
      else if (c = "\"") andalso countquotes mod 2 =1 andalso next = "\"" then ()
      else if (c = "\"") andalso countquotes mod 2 =1 andalso next= "\n"  then ()
      else if (c = "\"") andalso countquotes mod 2 =1 andalso next = delim1 then ()
      else if (c = "\"") andalso countquotes mod 2 =1 then raise ImproperDoubleQuotes
      else ()
    end end;

(* checks whether the number of fields are even in records *)
fun checkeven(countcurr, countmain, countrecord) = 
  if countmain = ~1 then ()
  else if countmain = countcurr then ()
  else raise UnevenFields("Expected: "^Int.toString(countmain)^" fields, Present: "^Int.toString(countcurr)^" fields on Line "^Int.toString(countrecord)^"\n");

(* to print a double quote before a delimeter or a line feed if needed *)
fun beforeD(inquotes, outs) = 
  if inquotes then () 
  else TextIO.output(outs,"\"");

(* to avoid prrinting a un0necessary double quotes in the very end *)
fun afterLF(ins,outs) = 
  case TextIO.lookahead(ins) of
    NONE => ()
  | SOME(c) => (TextIO.output(outs,"\""));
  
(* this functioon takes the character and performs some perform operations to print something in the output stream and maintain the other invariants  *)
fun printing(starting,inquotes,countquotes,countcurr,countmain,countrecord,c: string,delim1,delim2,ins,outs) =

  if starting andalso c= "\"" then (
    checkEOF(countquotes,ins,c);
    checkescapedquotes(ins,c, inquotes, delim1, countquotes)
  )
  else if ((c) = "\n") andalso (countquotes mod 2)=0 then (
    checkEOF(countquotes,ins,c);
    beforeD(inquotes, outs);
    TextIO.output(outs,"\n");
    afterLF(ins,outs);
    checkeven(countcurr+1,countmain,countrecord+1)
  )
  else if (c = delim1) andalso (countquotes mod 2)=0 then (
    checkEOF(countquotes,ins,c);
    beforeD(inquotes, outs);
    TextIO.output(outs,delim2);
    TextIO.output(outs,"\"")
  )
  else(
    TextIO.output(outs,c);
    checkEOF(countquotes,ins,c :string);
    checkescapedquotes(ins,c, inquotes, delim1, countquotes)
  )

fun helper(countquotes: int, countcurr: int, countmain: int, countrecord: int, starting: bool, inquotes: bool, c: string, delim1: string, delim2:string,ins,outs) =
  if c= "" then ( TextIO.closeIn ins; TextIO.closeOut outs )
  else ( printing(starting,inquotes,countquotes,countcurr,countmain,countrecord,c,delim1,delim2,ins,outs); helper(updatequotes(countquotes,c),updatecurr(countcurr,c,countquotes,delim1),updatemain(countcurr,countmain,countquotes,c),updaterecord(countquotes,c,countrecord),updatestarting(countquotes,c,delim1),updateinquotes(countquotes, c,inquotes,delim1,ins),updatec(ins),delim1,delim2,ins,outs) );


(* Final function for chaning the delimters *)

fun convertDelimiters(infilename,delim1,outfilename,delim2) = 
  let
    val ins = TextIO.openIn infilename
    val outs = TextIO.openOut outfilename
    
  in
    let
      val c = TextIO.lookahead(ins)
    in
      let val next = case c of NONE => "" | SOME(c) => str(c) in
      if next= "" then ( raise emptyInputFile)
      else if next= "\"" then (TextIO.output(outs,"\"") ; helper(0,0,~1,0,true,true,updatec(ins),str(delim1),str(delim2),ins,outs))
      else ( TextIO.output(outs,"\""); helper(0,0,~1,0,true,false,updatec(ins),str(delim1),str(delim2),ins,outs) )
      end
    end
  end
  handle UnevenFields(s) => print(s);

(* For Conversion between CSV and TSV *)
fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename,#",",outfilename,#"\t");
fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename,#"\t",outfilename,#",");

(* The overall time and space compexity is = O(N), N is the number of characters in the input file*)