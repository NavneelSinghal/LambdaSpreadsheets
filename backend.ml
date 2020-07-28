type f = Empty | Float of float
type sheet = f list list
type row = f list
type range = (int * int) * (int * int)
type index = (int * int)

exception Invalid_size
exception Invalid_range
exception Outside_sheet
exception Incompatible_ranges
exception Empty_cell
exception Wrong_input

(* Pointers - 
 * check if the input range/index is inside the sheet completely or not (and check if everything is inside the sheet or not (and if the output lies outside the sheet, then resize the sheet)
 * For the above check, we only need to find the output's bottom-rightmost cell and resize up to its coordinates + 1
 * Also whenever we are doing something on ranges, we need to check if they are compatible wrt size or not
 *)

(*Number of rows, number of columns*)
let dim (s : sheet) : (int * int) =
    match s with
    | [] -> (0, 0)
    | x :: xs -> ((List.length xs) + 1, List.length x);;

(*Returns a resized list l' which contains the first siz elements of l, and if siz > length of l, we keep appending the given element to l*)
let rec resize_list (l : 'a list) (siz : int) (element : 'a) : ('a list) = 
    if siz < 0 then raise Invalid_size
    else
    let rec resize_ (l_ : 'a list) (siz_ : int) (acc_ : 'a list) : ('a list) =
        if siz_ = 0 then acc_
        else 
            match l_ with 
            |   [] -> resize_ l_ (siz_ - 1) (element :: acc_)
            |   x_ :: xs_ -> resize_ xs_ (siz_ - 1) (x_ :: acc_)
    in List.rev (resize_ l siz []);;

(*Returns a resized sheet, uses the previous function twice, returns a list of x rows and y columns*)
let resize_sheet (s : sheet) (x : int) (y : int) : sheet = 
    let rec empty_of_length (siz : int) (acc : row) : row = 
        if siz = 0 then acc
        else empty_of_length (siz - 1) (Empty :: acc)
    in
    resize_list (List.map (fun l -> (resize_list l y Empty)) s) x (empty_of_length y []);;

(*Resizes along a dimension only if that input dimension is more than the extent of the sheet in that dimension*)
let resize_up (s : sheet) (x : int) (y : int) : sheet = 
    let (w, z) = dim s in resize_sheet s (max x w) (max y z);;

(*Resizes a sheet s up to the minimum size such that the coordinate (x, y) is inside the sheet*)
let resize_to_coordinate (s : sheet) ((x, y) : index) : sheet = 
    resize_up s (x + 1) (y + 1);;

(*Checks if a range is valid*)
let check_valid_range (((x, y), (z, w)) : range) : bool = 
    if (x <= z && y <= w) then true
    else raise Invalid_range;;

(*Checks if a range is valid and inside a sheet*)
let check_range_inside (s : sheet) (((x, y), (z, w)) : range) : bool = 
    if(check_valid_range (((x, y), (z, w)))) then 
        let (a, b) = dim s in 
            if (z < a && w < b) then true 
            else raise Outside_sheet
    else raise Invalid_range;;

(*Finds the extent of a given range*)
let extent_range (((x, y), (z, w)) : range) : (int * int) = 
    (z - x, w - y);;

(*Checks if the two input ranges are compatible*)
let check_compatible (r1 : range) (r2 : range) : bool = 
    if (extent_range r1 = extent_range r2) then true 
    else raise Incompatible_ranges;;

(*Checks if an index is inside a range*)
let check_index ((x, y) : index) (((a, b), (c, d)) : range) : bool = 
    (a <= x) && (x <= c) && (b <= y) && (y <= d);;

(*
 * Now we enumerate checks based on the types of the insructions 
 * 1. unary - we just have to check whether the range is inside the sheet (validity checked already), and work with the coordinate-resized version of s
 * 2. binary with range and range - we need to check whether both the ranges are inside the sheet and both are compatible and work with coordinate resized s
 * 3. binary with range and constant - we need to check if the range is inside the sheet and work with coordinate resized s, and also check if division by zero is done ever (confirm on piazza if division by 0 returns infinity or an error)
 * 
 * For binary instructions, we raise an exception when an operand is an empty cell
 *)

(*Let's make generic functions for accumulators*)
(*
 * 1. Finding something in a range and returning a number
 * 2. Finding something row-wise in a range and returning a list
 * 3. Finding something col-wise in a range and returning a list
 * 
 * 4. Setting a range equal to some grid - should handle the resizing up of the sheet too
 * 5. Extracting a grid out of a sheet - should probably be done first, also handles the inside_sheet thing about the range
 *)

(*Returns a list of elements in list l whose indices are between left and right both inclusive*)
let get_list_range (l : 'a list) (left : int) (right : int) : ('a list) = 
    let rec extract l_ i_ left_ right_ = 
        match l_ with 
        | [] -> []
        | x :: xs -> if (left_ <= i_ && i_ <= right) then x :: (extract xs (i_ + 1) left_ right_) else (extract xs (i_ + 1) left_ right_)
    in 
    extract l 0 left right;;

(*Returns a sheet of elements in sheet s which are in the specified range*)
let get_sheet_range (s : sheet) (r : range) : sheet = 
    if(check_range_inside s r) then
        match r with 
        ((x, y), (z, w)) -> List.map (fun l -> get_list_range l y w) (get_list_range s x z)
    else raise Outside_sheet;;

(*Returns the transpose of a sheet*)
let rec transpose (s : sheet) : sheet = (*enables us to focus on only the rows and use this function for columns*)
    match s with 
    | [] -> []
    | [] :: xs -> []
    | x :: xs -> (List.map List.hd s) :: (transpose (List.map List.tl s));;

(*Reduces all rows using a given function and then returns a single row (not a sheet)*)
let rec reduce_sheet_to_one_row func element s = 
    match s with 
    | [] -> []
    | x :: xs -> (List.fold_left func element x) :: (reduce_sheet_to_one_row func element xs);;

(*This gives something in range format - computes row-wise reduction and gives a single-column sheet as an answer*)
let rec reduce_sheet_rows_to_col func element s = transpose [(reduce_sheet_to_one_row func element s)];;

(*This gives something in range format - computes column-wise reduction and gives a single-row sheet as an answer*)
let rec reduce_sheet_cols_to_row func element s = [reduce_sheet_to_one_row func element (transpose s)];;

(*This gives result as a sheet with only one element - first applies one function to each row then applies the second to the resulting row*)
let rec reduce_sheet_to_singleton func1 func2 element s = [reduce_sheet_to_one_row func2 element [reduce_sheet_to_one_row func1 element s]];;

(*Given a list l and a list m, this function returns a list that has elements from index left to right in l replaced by the elements of m, correct if left and right are both indices that exist in the list l, undefined behaviour otherwise*)
let set_range (l : 'a list) (m : 'a list) (left : int) (right : int) = 
    if(right - left + 1 = List.length m) then
        let rec set_range_ l_ m_ i_ left_ right_ = 
            match l_ with 
            | [] -> []
            | x_ :: xs_ -> 
                    if (left_ <= i_) && (i_ <= right_) then 
                        match m_ with
                        | [] -> raise Incompatible_ranges
                        | y_ :: ys_ -> y_ :: (set_range_ xs_ ys_ (i_ + 1) left_ right_)
                    else 
                        x_ :: set_range_ xs_ m_ (i_ + 1) left_ right_
        in
        set_range_ l m 0 left right
    else raise Incompatible_ranges;;

(*Sets the elements in the specified range of s to be the elements of t*)
let set_range_sheet (s : sheet) (t : sheet) (((x, y), (z, w)) : range) = 
    set_range s (List.map2 (fun x_ y_ -> set_range x_ y_ y w) (get_list_range s x z) t) x z;;

(*This is the function to set the elements in a given range in s to the elements in t, resizing if needed*)
let set_range_expand (s : sheet) (t : sheet) (((x, y), (z, w)) : range) = 
    set_range_sheet (resize_to_coordinate s (z, w)) t ((x, y), (z, w));; 

(*This is a function to generate a list of size x with all elements equal to element*)
let rec fill_row x element = 
    if(x < 0) then raise Wrong_input
    else if x = 0 then [] else element :: (fill_row (x - 1) element);;

Random.self_init();;

(*This is a function to generate a random row*)
let rec fill_row_random x =
    if(x < 0) then raise Wrong_input
    else if x = 0 then [] else (Float (Random.float 10.)) :: (fill_row_random (x - 1));;

(*This returns a sheet with dimensions (x, y) filled with Float(num)*)
let rec fill_sheet (x, y) num =
    fill_row x (fill_row y (Float(num)));;

let rec fill_sheet_random (x, y) = 
    if(x < 0) then raise Wrong_input
    else if x = 0 then [] else (fill_row_random y) :: (fill_sheet_random (x - 1, y));;

(*This is a function which is used to find the count of cells*)
let count_cell x y = 
    match (x, y) with
    | (Float(a), Float(b)) -> Float(a +. 1.)
    | (Float(a), Empty) -> Float(a)
    | _ -> raise Empty_cell;;

(*This is a function which is used to find the sum of elements in a cell*)
let add_cell x y = 
    match (x, y) with
    | (Float(a), Float(b)) -> Float(a +. b)
    | _ -> raise Empty_cell;;

(*This is a function which is used to find the minimum of two numbers of type f*)
let min_float a b = 
    match (a, b) with
    | (Float(x), Float(y)) -> if x < y then Float(x) else Float(y)
    | _ -> raise Empty_cell;;

(*This is a function which is used to find the maximum of two numbers of type f*)
let max_float a b = 
    match (a, b) with
    | (Float(x), Float(y)) -> if x < y then Float(y) else Float(x)
    | _ -> raise Empty_cell;;

(*This is an analogue of the List.map2 function for sheets*)
let map2_sheet func s1 s2 = 
    try
        List.map2 (fun l1 l2 -> List.map2 func l1 l2) s1 s2
    with
        Invalid_argument(x) -> raise Incompatible_ranges;;

(*This adds 2 sheets*)
let add_sheets s1 s2 = 
    map2_sheet (fun x y -> (match (x, y) with (Float(a), Float(b)) -> Float(a +. b) | _ -> raise Empty_cell)) s1 s2;;

let sub_sheets s1 s2 = 
    map2_sheet (fun x y -> (match (x, y) with (Float(a), Float(b)) -> Float(a -. b) | _ -> raise Empty_cell)) s1 s2;;

let mult_sheets s1 s2 = 
    map2_sheet (fun x y -> (match (x, y) with (Float(a), Float(b)) -> Float(a *. b) | _ -> raise Empty_cell)) s1 s2;;

let div_sheets s1 s2 = 
    map2_sheet (fun x y -> (match (x, y) with (Float(a), Float(b)) -> Float(a /. b) | _ -> raise Empty_cell)) s1 s2;;

let add_sheet_num s1 num = add_sheets s1 (fill_sheet (dim s1) num);;

let sub_sheet_num s1 num = sub_sheets s1 (fill_sheet (dim s1) num);;

let mult_sheet_num s1 num = mult_sheets s1 (fill_sheet (dim s1) num);;

let div_sheet_num s1 num = div_sheets s1 (fill_sheet (dim s1) num);;

let infer_range (x, y) input_range op_type = 
    if op_type = "singleton" then ((x, y), (x, y))
    else if op_type = "row" then 
        match extent_range input_range with
        (diff, _) -> ((x, y), (x + diff, y))
    else if op_type = "col" then 
        match extent_range input_range with 
        (_, diff) -> ((x, y), (x, y + diff))
    else 
        match extent_range input_range with
        (xdiff, ydiff) -> ((x, y), (x + xdiff, y + ydiff));;

let unary_on_sheet s r i op_type func1 func2 element =
    let answer = 
        if(op_type = "singleton") then reduce_sheet_to_singleton func1 func2 element (get_sheet_range s r)
        else if op_type = "row" then reduce_sheet_rows_to_col func1 element (get_sheet_range s r)
        else if op_type = "col" then reduce_sheet_cols_to_row func1 element (get_sheet_range s r)
        else raise Wrong_input
    in
    set_range_expand s answer (infer_range i r op_type);; 

let binary_on_sheet s r1 r2 i constant op_type =
    let answer =
        let (range1, range2) = ((get_sheet_range s r1), (get_sheet_range s r2)) in
        if(op_type = "constant add") then add_sheet_num range1 constant
        else if op_type = "constant sub" then sub_sheet_num range1 constant
        else if op_type = "constant mult" then mult_sheet_num range1 constant
        else if op_type = "constant div" then div_sheet_num range1 constant
        else if op_type = "range add" then add_sheets range1 range2
        else if op_type = "range sub" then sub_sheets range1 range2
        else if op_type = "range mult" then mult_sheets range1 range2
        else if op_type = "range div" then div_sheets range1 range2
        else raise Wrong_input
    in
    set_range_expand s answer (infer_range i r1 op_type);;

let unary_avg s r i op_type = 
    let (a1, a2) = 
        if(op_type = "singleton") then ((reduce_sheet_to_singleton count_cell add_cell (Float(0.)) (get_sheet_range s r)), (reduce_sheet_to_singleton add_cell add_cell (Float(0.)) (get_sheet_range s r)))
        else if op_type = "row" then ((reduce_sheet_rows_to_col count_cell (Float 0.) (get_sheet_range s r)), (reduce_sheet_rows_to_col add_cell (Float 0.) (get_sheet_range s r)))
        else if op_type = "col" then ((reduce_sheet_cols_to_row count_cell (Float 0.) (get_sheet_range s r)), (reduce_sheet_cols_to_row add_cell (Float 0.) (get_sheet_range s r)))
        else raise Wrong_input
    in
    set_range_expand s (div_sheets a2 a1) (infer_range i r op_type);;

let full_count: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "singleton" count_cell add_cell (Float(0.));;

let row_count: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "row" count_cell add_cell (Float(0.));;    

let col_count: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "col" count_cell add_cell (Float(0.));;

let full_sum: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "singleton" add_cell add_cell (Float(0.));;

let row_sum: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "row" add_cell add_cell (Float(0.));;

let col_sum: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "col" add_cell add_cell (Float(0.));;

let full_avg: sheet -> range -> index -> sheet = fun a b c -> unary_avg a b c "singleton";;

let row_avg: sheet -> range -> index -> sheet = fun a b c -> unary_avg a b c "row";;

let col_avg: sheet -> range -> index -> sheet = fun a b c -> unary_avg a b c "col";;

let full_min: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "singleton" min_float min_float (Float(infinity));;

let row_min: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "row" min_float min_float (Float(infinity));;

let col_min: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "col" min_float min_float (Float(infinity));;

let full_max: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "singleton" max_float max_float (Float(neg_infinity));;

let row_max: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "row" max_float max_float (Float(neg_infinity));;

let col_max: sheet -> range -> index -> sheet = fun a b c -> unary_on_sheet a b c "col" max_float max_float (Float(neg_infinity));;

let add_const: sheet -> range -> float -> index -> sheet = fun a b c d -> binary_on_sheet a b b d c "constant add";;

let subt_const: sheet -> range -> float -> index -> sheet = fun a b c d -> binary_on_sheet a b b d c "constant sub";; 

let mult_const: sheet -> range -> float -> index -> sheet = fun a b c d -> binary_on_sheet a b b d c "constant mult";;

let div_const: sheet -> range -> float -> index -> sheet  = fun a b c d -> binary_on_sheet a b b d c "constant div";;

let add_range: sheet -> range -> range -> index -> sheet = fun a b c d -> binary_on_sheet a b c d 0. "range add";;

let subt_range: sheet -> range -> range -> index -> sheet = fun a b c d -> binary_on_sheet a b c d 0. "range sub";; 

let mult_range: sheet -> range -> range -> index -> sheet = fun a b c d -> binary_on_sheet a b c d 0. "range mult";;

let div_range: sheet -> range -> range -> index -> sheet = fun a b c d -> binary_on_sheet a b c d 0. "range div";;

