#use "types.ml";;

let instptr_mk cmd_list = 
	let rec aux cmd_list acc =
		match (List.tl cmd_list) with
		|[] -> [acc, (List.hd cmd_list)]
		|_ -> [acc, (List.hd cmd_list)]@(aux (List.tl cmd_list) (acc+1))
	in InstPtr([],(aux cmd_list 0));;

(* let instptr_mk' cmd_list =
	let n = List.length cmd_list in
	let ls = ints(n-1) in
	let iis = List.combine ls cmd_list in
	InstPtr([], iis);; *)

let instptr_move_up' instptr = function
	|InstPtr(x::xs, xs')->InstPtr(xs, x::xs') (*x::xs permet de regarder si il y un élément avant dans la liste*)
	|instptr->iptr;;

let instptr_move_up inst_ptr = 
	match inst_ptr with
	| InstPtr(_, []) -> inst_ptr
	| InstPtr(list1, list2) ->  InstPtr((List.tl list1), (List.hd list1)::list2);;

let instptr_move_down instptr = function
	|InstPtr(_, []) -> instptr
	|InstPtr(list1, list2) -> InstPtr((List.hd list2)::list1, (List.tl list2));;

let instptr_get inst_ptr = 
	match inst_ptr with
	| InstPtr(_,[]) -> failwith "Commande non valide"
	| InstPtr(_, list2) ->(List.hd list2);;



instptr_get(instptr_move_up (instptr_move_down (instptr_mk add_program)));;