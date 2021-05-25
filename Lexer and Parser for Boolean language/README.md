## Steps to run the code:

1. Clone the repository and Open Terminal in the directory "Lexer and Parser for Boolean language".  
( Required files => ML-Lex.lex, ML-Yacc.yacc, Makefile, loader.sml, load-bool.sml) 
2. Run the `make all` command in terminal.
3. Now run the command `parseString <file_name>` to lex and parse the required file. <file_name> is the name of Required input file and must be enclosed in double quotes. Make sure the input file is present in the same directory.
4. The first line of output is the output of Lexer and second line of the output is the output of parser.

Note: In case of unknown token the exception is thrown and the ouput of terminal terminates, the input before that must be ignored and input of parser after that must also be ignored.