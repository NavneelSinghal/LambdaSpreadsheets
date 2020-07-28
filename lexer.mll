{
    open Parser1
    (*documentation as in the previous assignment and parser1.mly*)
}

let space = [' ' '\t']
let spaceline = [' ' '\t' '\n']
let sign = ['-' '+']
let digit = ['0'-'9']
let nzdigit = ['1'-'9']
let integer = digit|(nzdigit digit*)
let fracpart = '.' ((digit* nzdigit)|digit)
let unop = "COUNT" | "ROWCOUNT" | "COLCOUNT" | "SUM" | "ROWSUM" | "COLSUM" | "AVG" | "ROWAVG" | "COLAVG" | "MIN" | "ROWMIN" | "COLMIN" | "MAX" | "ROWMAX" | "COLMAX"
let binop = "ADD" | "SUBT" | "MULT" | "DIV"

rule token = parse

| (sign? integer fracpart) as number {Float(float_of_string number)}

| '(' space* '['  space* (integer as n1) space* ',' space* (integer as n2) space* ']' space* ':' space* '[' space* (integer as n3) space* ',' space* (integer as n4) space* ']' space* ')' {Range(((int_of_string n1), (int_of_string n2)), ((int_of_string n3), (int_of_string n4)))}

| '['  space* (integer as n1) space* ',' space* (integer as n2) space* ']' {Index((int_of_string n1), (int_of_string n2))}

| '(' {Lparen}
| ')' {Rparen}
| '[' {Lbrac}
| ']' {Rbrac}
| ',' {Comma}
| ":=" {Assignment}
| ':' {Colon}

| unop as operator {UnOp (operator)}

| binop as operator {BinOp (operator)}

| ';' {Termination}
| spaceline {token lexbuf}
| eof {EOF}
