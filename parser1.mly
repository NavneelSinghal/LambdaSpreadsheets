%{
    open Types
%}

    /*Tokens are the same as assignment 2, except that they are now in the parsing file (this file) and an end of file is a token and not an exception*/
%token <float> Float
%token <int*int> Index
%token <(int*int)*(int*int)> Range
%token Assignment Termination Lparen Rparen Lbrac Rbrac Colon Comma
%token <string> UnOp BinOp
%token EOF

    /*start state is parse*/
%start parse
    /*parse and instructions are lists of commands, which is a type as given in types.ml, and instruction is of type command*/
%type <Types.command list> parse, instructions
%type <Types.command> instruction

%%
    
parse:          instructions EOF         {$1}
            ;
    /*left recursive definition for instructions, which returns the list of commands in reverse order*/
instructions:                            {[]}
            |   instructions instruction {$2 :: $1} 
            ;

instruction:    Index Assignment UnOp Range Termination 
                {
                    (*Found a unary operator*)
                    Unary($3, $1, $4)
                }
            |   Index Assignment BinOp Range Range Termination 
                {
                    (*Found a binary operator, range, range*)
                    Binaryrr($3, $1, $4, $5)
                }
            |   Index Assignment BinOp Range Float Termination
                {
                    (*Found a binary operator, range, constant*)
                    Binaryrc($3, $1, $4, $5)
                }
            |   Index Assignment BinOp Float Range Termination
                {
                    (*Found a binary operator, constant, range*)
                    Binarycr($3, $1, $4, $5)
                }
            |   Index Assignment BinOp Index Range Termination
                {
                    (*Found a binary operator, index, range*)
                    Binaryir($3, $1, $4, $5)
                }
            |   Index Assignment BinOp Range Index Termination
                {
                    (*Found a binary operator, range, index*)
                    Binaryri($3, $1, $4, $5)
                }
            ;
%%

