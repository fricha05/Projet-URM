exception Syntax_error

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

(* let program_of_lex_eurm lex =
    match lex with
    match lex with
    |[] -> []
    |"Zero" :: arg_1 :: tail ->
        (Zero (int_of_string arg_1)) :: (program_of_lex tail)
    |"Succ" :: arg_1 :: tail ->
        (Succ (int_of_string arg_1)) :: (program_of_lex tail)
    |"Copy" :: arg_1 :: arg_2 :: tail ->
        (Copy ((int_of_string arg_1), (int_of_string arg_2)))
        :: (program_of_lex tail)
    |"Jump" :: arg_1 :: arg_2 :: arg_3 :: tail ->
        (URMJump ((int_of_string arg_1), (int_of_string arg_2),
        (int_of_string arg_3)))
        :: (program_of_lex tail)
    |_ -> raise Syntax_error *)
