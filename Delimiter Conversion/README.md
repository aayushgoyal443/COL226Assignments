## Steps to run the code:

1. Clone the repository and change directory to "Delimiter Conversion".  
(That is the directory in which "csv2dsv.sml" is present)
2. Run the `make all` command in terminal.
3. Now run the command `convertDelimiters(<input_file>,<delimiter1>,<output_file>,<delimiter2>)` to convert the "delimiter1" used in the file "input_file" into "delimiter2" and save it in "output_file".  
Note: Filename should be in double quotes. delimiter character should be in format #"delimiter", eg #"," for comma.
4. The <output_file> contains the required delimiter separated file.