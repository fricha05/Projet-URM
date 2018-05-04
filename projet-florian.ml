#use "types.ml"

let rec compile_comment_out is =
    match is with
    |[] -> []
    |Comment s::is' -> compile_comment_out is'
    |i::is' -> i::compile_comment_out is'
;;

let compile_comment_out' is =
    let is_not_comment i =
        match i with
        |Comment _ -> false
        |_ -> true
        in List.filter is_not_comment is
;;

let rec compile_label_out is =
	let rec aux is acc =
		match is with (* is: instructions *)
		|[] -> []
		|Label s::is' -> Label("Label " ^ string_of_int acc) :: (aux is' (acc+1))
		|i::is' -> i::(aux is' acc)
	in aux is 1;
;;

let compile_preprocess is = compile_label_out(compile_comment_out' is);;

let rec compile_regs_out is =
    match is with
    |[] -> []
    |Add(idx, idy) ->
