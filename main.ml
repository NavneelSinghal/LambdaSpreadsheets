open Backend
open Types
open Str

exception Wrong_instruction
exception OutOfBounds
exception EmptyCell

(*Function to get the ith element of a list s (throws OutOfBounds if the index i is out of bounds)*)
let rec get_ s i =  
    if i = 0 then 
        match s with 
        | [] -> raise OutOfBounds
        | x :: xs -> x
    else if i > 0 then
        match s with 
        | [] -> raise OutOfBounds
        | x :: xs -> (get_ xs (i - 1))
    else raise OutOfBounds

(*Function to get element at (i, j) in a list of lists s*)
let get s (i, j) = get_ (get_ s i) j

(*Function to print a list in a line*)
let rec print_list (a : f list) = 
    match a with
    | [] -> Printf.printf "\n"
    | Float(x) :: xs -> Printf.printf "%f " x; print_list xs
    | Empty :: xs -> Printf.printf "Empty "; print_list xs

(*Function to print a list of lists as a grid*)
let rec print_sheet s = 
    match s with
    | [] -> ()
    | x::xs -> print_list x; print_sheet xs

(*Function to run a unary operation*)
let run_unary s operator start range = 
    if operator = "COUNT" then full_count s range start 
    else if operator = "ROWCOUNT" then row_count s range start
    else if operator = "COLCOUNT" then col_count s range start
    else if operator = "SUM" then full_sum s range start
    else if operator = "ROWSUM" then row_sum s range start
    else if operator = "COLSUM" then col_sum s range start
    else if operator = "AVG" then full_avg s range start
    else if operator = "ROWAVG" then row_avg s range start
    else if operator = "COLAVG" then col_avg s range start
    else if operator = "MIN" then full_min s range start
    else if operator = "ROWMIN" then row_min s range start
    else if operator = "COLMIN" then col_min s range start
    else if operator = "MAX" then full_max s range start
    else if operator = "ROWMAX" then row_max s range start
    else if operator = "COLMAX" then col_max s range start
    else raise Wrong_instruction

(*Function to run a binary operator on 2 ranges*)
let run_binary_range_range s operator start range1 range2 = 
    if operator = "ADD" then add_range s range1 range2 start
    else if operator = "SUBT" then subt_range s range1 range2 start
    else if operator = "MULT" then mult_range s range1 range2 start
    else if operator = "DIV" then div_range s range1 range2 start
    else raise Wrong_instruction

(*Function to run a binary operator on a range and a constant*)
let run_binary_range_constant s operator start range constant = 
    if operator = "ADD" then add_const s range constant start
    else if operator = "SUBT" then subt_const s range constant start
    else if operator = "MULT" then mult_const s range constant start
    else if operator = "DIV" then div_const s range constant start
    else raise Wrong_instruction 
    
(*Function to run a binary operator on a constant and a range*)
let run_binary_constant_range s operator start constant range = 
    run_binary_range_constant s operator start range constant

(*Function to run a binary operator on an index and a range*)
let run_binary_index_range s operator start index range = 
    let const = get s index in
        match const with
        | Float(constant) -> run_binary_constant_range s operator start constant range
        | Empty -> raise EmptyCell

(*Function to run a binary operator on a range and an index*)
let run_binary_range_index s operator start range index = 
    let const = get s index in
        match const with 
        | Float(constant) -> run_binary_range_constant s operator start range constant
        | Empty -> raise EmptyCell

(*Function to run any function of type command*)
let eval s instruction = 
    match instruction with
    | None -> s
    | Unary(operator, start, range) -> run_unary s operator start range
    | Binaryrr(operator, start, range1, range2) -> run_binary_range_range s operator start range1 range2
    | Binaryri(operator, start, range, index) -> run_binary_range_index s operator start range index
    | Binaryrc(operator, start, range, constant) -> run_binary_range_constant s operator start range constant
    | Binaryir(operator, start, index, range) -> run_binary_index_range s operator start index range
    | Binarycr(operator, start, constant, range) -> run_binary_constant_range s operator start constant range
        
(*Function to run a list of commands a on a sheet s*)
let rec eval_list a s = 
    match a with 
        | [] -> s
        | x :: xs -> try
                        print_sheet s; Printf.printf "\n"; eval_list xs (eval s x)
                    with e -> 
                        let message = Printexc.to_string e 
                        and stack = Printexc.get_backtrace () in
                        Printf.printf "Invalid instruction, continuing evaluation on the remaining instructions.\nException raised: %s%s\n\n" message stack; eval_list xs s;; 

let comma = regexp ",";;
let parse_line line = List.map (fun x -> if x = "" then Empty else Float (float_of_string x)) (split_delim comma line);;
let parse_csv filename = 
    let inp = open_in filename in
    let rec rev_list_of_csv acc =
        try
            let line = input_line inp in
            rev_list_of_csv ((parse_line line)::acc)
        with
        | e -> acc
    in List.rev (rev_list_of_csv []);;

let s1 = fill_sheet_random (10, 10) 
let s = parse_csv Sys.argv.(1)

(*Driver code*)
let _  =
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(4)) in
        Printf.printf "Starting parsing\n\n";
        let ls = List.rev (Parser1.parse Lexer.token lexbuf) in 
            begin
                Types.print_command_list ls; 
                Printf.printf "\nEnded parsing\n\nStarting execution\n\n";
                let fin = (eval_list ls s) in 
                begin
                    Printf.printf "Final state of sheet:\n\n";
                    print_sheet fin;
                end;
            end;
        Printf.printf "\nExecution completed\n"
