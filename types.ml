(*Each command is encoded as a command in the yacc file, and that type is as follows*)
type command = None (*A dummy constructor used for debugging purposes - this allowed for an incremental development of functions used in the eval function*)
             | Unary of string * (int*int) * ((int*int)*(int*int)) (*A unary operator*)
             | Binaryrr of string * (int*int) * ((int*int)*(int*int)) * ((int*int)*(int*int)) (*A binary operator with both arguments used in the computation being ranges*)
             | Binaryri of string * (int*int) * ((int*int)*(int*int)) * (int*int) (*Same as above, but the first is a range and second is an index*)
             | Binaryir of string * (int*int) * (int*int) * ((int*int)*(int*int)) (*Same as above, but the first is an index and second is a range*)
             | Binaryrc of string * (int*int) * ((int*int)*(int*int)) * float (*Same as above, but the first is a range and the second is a floating point constant*)
             | Binarycr of string * (int*int) * float * ((int*int)*(int*int)) (*Same as above, but the first is a floating point constant and the second is a range*)

(*A function to print an index*)
let printi x =
    match x with (w, z) -> Printf.printf "Index(%d, %d)" w z
    
(*A function to print a range*)
let printr x = 
    match x with (w, z) -> Printf.printf "Range("; printi w; Printf.printf ", "; printi z; Printf.printf ")"

(*A function to print a command of type command*)
let print_command c = 
    match c with 
    | None -> Printf.printf "No command\n"
    | Unary(s, i, r) -> Printf.printf "%s " s; printi i; Printf.printf " "; printr r; Printf.printf "\n"
    | Binaryrr(s, i, r1, r2) -> Printf.printf "%s " s; printi i; Printf.printf " "; printr r1; Printf.printf " "; printr r2; Printf.printf "\n"
    | Binaryri (s, i, r, i2) -> Printf.printf "%s " s; printi i; Printf.printf " "; printr r; Printf.printf " "; printi i2; Printf.printf "\n"
    | Binaryir (s, i, i2, r) -> Printf.printf "%s " s; printi i; Printf.printf " "; printi i2; Printf.printf " "; printr r; Printf.printf "\n"
    | Binaryrc (s, i, r, c) -> Printf.printf "%s " s; printi i; Printf.printf " "; printr r; Printf.printf " %f\n" c
    | Binarycr (s, i, c, r) -> Printf.printf "%s " s; printi i; Printf.printf " %f " c; printr r; Printf.printf "\n"

(*A function used to print lists of commands*)
let rec print_command_list l = 
    match l with
    | [] -> ()
    | x :: xs -> print_command x; print_command_list xs
