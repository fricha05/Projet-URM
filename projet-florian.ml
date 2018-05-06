

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
		|[] -> [] (*, State("0",acc)*)
		|Label s::is' -> Label(string_of_int acc) :: (aux is' (acc+1)) (* "Label " ^  *)
		|i::is' -> i::(aux is' acc)
	in aux is 1;
;;

let rec create_state_label is =
	let rec aux is acc =
		match is with
		|[] -> string_of_int acc
		|Label s::is' -> aux is' (acc+1) 
		|i::is' -> (aux is' acc)
	in aux is 1;
;;

let max a b = if (a > b) then a else b;;
let max_ter a b c = max (max a b) c ;;

let rec create_state_reg is =
	let rec aux is regid =
		match is with
		|[] -> (regid+1)
		|Add(r1,r2)::is' -> aux is' (max_ter r1 r2 regid) 
		|Copy(r1,r2)::is' -> aux is' (max_ter r1 r2 regid) 
		|Dec(r1)::is' -> aux is' (max r1 regid) 
		|EqPredicate(r1,r2,l)::is' -> aux is' (max_ter r1 r2 regid) 
		|GEqPredicate(r1,r2,l)::is' -> aux is' (max_ter r1 r2 regid) 
		|GTPredicate(r1,r2,l)::is' -> aux is' (max_ter r1 r2 regid) 
		|Inc r1::is' -> aux is' (max r1 regid) 
		|LEqPredicate(r1,r2,l)::is' -> aux is' (max_ter r1 r2 regid) 
		|LTPredicate(r1,r2,l)::is' -> aux is' (max_ter r1 r2 regid) 
		|Mult(r1,r2)::is' -> aux is' (max_ter r1 r2 regid) 
		|Sub(r1,r2)::is' -> aux is' (max_ter r1 r2 regid) 
		|Zero r1::is' -> aux is' (max r1 regid) 
		|ZeroPredicate(r1,l)::is' -> aux is' (max r1 regid) 
		|i::is' -> (aux is' regid)
	in aux is 0
;;

let create_state is = (is, State(create_state_label is, create_state_reg is));;

(* ajoute le registre maximum + 1 au state 
let compile_state_register *)

(* nombre de registres maximum + 1 : state *)

let compile_preprocess is = create_state (compile_label_out(compile_comment_out' is));;

(*
Non nécessaire

let rec compile_regs_out is =
    match is with
    |[] -> []
    |Add(idx, idy) ->
*)