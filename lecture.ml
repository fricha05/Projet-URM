exception Syntax_error

(* ---- URM ---- *)

let rec string_of_file f =
    try
        let str = input_line f in
            str ^ " " ^ (string_of_file f)
        with
            | End_of_file -> ""


let rec program_of_lex lex =
    match lex with
    |[] -> []
    |"Zero" :: arg_1 :: tail ->
        (URMZero (int_of_string arg_1)) :: (program_of_lex tail)
    |"Succ" :: arg_1 :: tail ->
        (URMSucc (int_of_string arg_1)) :: (program_of_lex tail)
    |"Copy" :: arg_1 :: arg_2 :: tail ->
        (URMCopy ((int_of_string arg_1), (int_of_string arg_2)))
        :: (program_of_lex tail)
    |"Jump" :: arg_1 :: arg_2 :: arg_3 :: tail ->
        (URMJump ((int_of_string arg_1), (int_of_string arg_2),
        (int_of_string arg_3)))
        :: (program_of_lex tail)
    |_ -> raise Syntax_error

let program_of_string str =
    let lex = Str.split (Str.regexp "[ \t\n(),]+") str in
        List.iter (fun s -> print_string s; print_newline ()) lex;
        program_of_lex lex

(*---- EURM ----*)

let rec program_of_lex_eurm lex =
    match lex with
    |[] -> []
    |"Zero" :: arg_1 :: tail ->
        (Zero (int_of_string arg_1)) :: (program_of_lex_eurm tail)
    |"Inc" :: arg_1 :: tail ->
        (Inc (int_of_string arg_1)) :: (program_of_lex_eurm tail)
    |"Dec" :: arg_1 :: tail ->
        (Dec (int_of_string arg_1)) :: (program_of_lex_eurm tail)
    |"Add" :: arg_1 :: arg_2 :: tail ->
        (Add ((int_of_string arg_1), (int_of_string arg_2)))
        :: (program_of_lex_eurm tail)
    |"Sub" :: arg_1 :: arg_2 :: tail ->
        (Sub ((int_of_string arg_1), (int_of_string arg_2)))
        :: (program_of_lex_eurm tail)
    |"Copy" :: arg_1 :: arg_2 :: tail ->
        (Copy ((int_of_string arg_1), (int_of_string arg_2)))
        :: (program_of_lex_eurm tail)
    |"Mult" :: arg_1 :: arg_2 :: tail ->
        (Mult ((int_of_string arg_1), (int_of_string arg_2)))
        :: (program_of_lex_eurm tail)
    |"ZeroPredicate" :: arg_1 :: arg_2 :: tail ->
        (ZeroPredicate ((int_of_string arg_1), (arg_2)))
        :: (program_of_lex_eurm tail)
    |"EqPredicate" :: arg_1 :: arg_2 :: arg_3 :: tail ->
        (EqPredicate ((int_of_string arg_1), (int_of_string arg_2),
        (arg_3)))
        :: (program_of_lex_eurm tail)
    |"GEqPredicate" :: arg_1 :: arg_2 :: arg_3 :: tail ->
        (GEqPredicate ((int_of_string arg_1), (int_of_string arg_2),
        (arg_3)))
        :: (program_of_lex_eurm tail)
    |"GTPredicate" :: arg_1 :: arg_2 :: arg_3 :: tail ->
        (GTPredicate ((int_of_string arg_1), (int_of_string arg_2),
        (arg_3)))
        :: (program_of_lex_eurm tail)
    |"LEqPredicate" :: arg_1 :: arg_2 :: arg_3 :: tail ->
        (LEqPredicate ((int_of_string arg_1), (int_of_string arg_2),
        (arg_3)))
        :: (program_of_lex_eurm tail)
    |"LTPredicate" :: arg_1 :: arg_2 :: arg_3 :: tail ->
        (LTPredicate ((int_of_string arg_1), (int_of_string arg_2),
        (arg_3)))
        :: (program_of_lex_eurm tail)
    |"Label" :: arg_1 :: tail ->
        (Label (arg_1)) :: (program_of_lex_eurm tail)
    |"Comment" :: arg_1 :: tail ->
        (Comment (arg_1)) :: (program_of_lex_eurm tail)
    |"Goto" :: arg_1 :: tail ->
        (Goto (arg_1)) :: (program_of_lex_eurm tail)
    |"Quit" :: tail ->
        (Quit :: (program_of_lex_eurm tail))
    |_ -> raise Syntax_error

let rec string_of_file_eurm f =
   try
        let str = input_line f in
            str::(string_of_file_eurm f)
        with
            |End_of_file -> []
;;

let lines_from_file file =
   let aux l = List.map (fun s -> Str.split (Str.regexp "[ ]+") s) l
in aux (string_of_file_eurm (open_in file))
;;

(* let program_of_string_eurm str =
    let lex = Str.split (Str.regexp "[ \t\n(),]+") str in
        List.iter (fun s -> print_string s; print_newline ()) lex;
        program_of_lex_eurm lex *)
