
#use "types.ml";;

(* 1.3.1 Pointeur sur instruction *)

(* Create an instruction pointer for an URM program. *)
(* val instptr_mk : urmcmd list -> instptr = <fun> *)
let instptr_mk cmd_list =
	let rec aux cmd_list acc =
		match (List.tl cmd_list) with
		| [] -> [acc, (List.hd cmd_list)]
		| _ -> [acc, (List.hd cmd_list)]@(aux (List.tl cmd_list) (acc+1))
	in InstPtr([],(aux cmd_list 0))
;;

let get_instptr_line instptr =
	match instptr with
	|InstPtr(_, list2) ->
		match List.hd list2 with
		|(line, _) -> line
		|_ -> failwith "error"
	|_->failwith "error"
;;

(* ( [], [(0, ZERO O), (1, JUMP 1 3 6), ...] ) *)

(* Move the instruction pointer up. Do nothing if this is not possible. *)
(* val instptr_move_up : instptr -> instptr = <fun> *)
let instptr_move_up inst_ptr =
	match inst_ptr with
	| InstPtr(list1, list2) ->
		if list2 <> []
			then InstPtr( (list1@(List.tl list2)), (List.tl list2) )
		else
			InstPtr(list1, list2)
;;

let instptr_move_up inst_ptr =
	match inst_ptr with
	| InstPtr(_, []) -> inst_ptr
	| InstPtr(list1, list2) ->  InstPtr( ((List.tl list1),(List.hd list1)::list2) )
;;

let instptr_move_down instptr =
	match instptr with
	| InstPtr(_, []) -> instptr
	| InstPtr(list1, list2) -> InstPtr((List.hd list2)::list1, (List.tl list2));;

(* Get the current command from the instruction pointer. *)
(* Fail if the command pointer is not set on a valid command. *)
(* val instptr_get : instptr -> line * urmcmd = <fun> *)
(* on récupère le premier élément de la dernière liste *)

let instptr_get inst_ptr =
	match inst_ptr with
	| InstPtr(_,[]) -> failwith "Commande non valide"
	| InstPtr(_, list2) -> (List.hd list2)
;;

(* Get the current instruction as a string.
 * Returns "null" is the instruction pointer is not valid. *)
(* val instptr_string : instptr -> string = <fun> *)

let instptr_string inst_ptr =
	match (instptr_get inst_ptr) with
	| (_, Copy(i,j)) -> "Copy " ^ string_of_int i
	| (_, Jump(i, j, k)) -> "Jump " ^ string_of_int(i) ^ " " ^ string_of_int(j) ^ " " ^ string_of_int(k)
	| (_, Succ(i)) -> "Succ"
	| (_, Zero(i)) -> "Zero"
;;

(*instptr_valid*)
instptr_get(instptr_move_up (instptr_move_down (instptr_mk add_program)));;
instptr_string(instptr_move_down (instptr_move_down (instptr_mk add_program)));;


(* Returns the index of a register. *)
(* val reg_idx : reg -> regidx = <fun> *)

let reg_idx reg =
	match reg with
	| Reg(index,_) -> index
;;

(* Compares two register Ri and Rj.
 * It returns an integer less than, equal to, or greater than zero if
 * the first register index is respectively less than, equal to, or
 * greater than the second register index. *)
(* val reg_compar : reg -> reg -> int = <fun> *)

let reg_compar reg1 reg2 =
	match reg1, reg2 with
	| Reg(_,val1), Reg(_,val2) ->
		if val1 < val2
			then (-1)
		else if val1 = val2
			then 0
		else
			1
;;

(* Returns the register value of a register from its index. Fails if
 * there is not register with the sought register index. *)
(* val get_regval : reg list -> regidx -> regval = <fun> *)

(* Recupere valeur à un indice du registre *)

let rec regs_get_value_from_idx regList reg_idx =
	if regList = [] (* si liste vite : erreur *)
		then failwith "failed"
	else
	match (List.hd regList) with
	| Reg(index, value) ->
		if index = reg_idx (* si on est au bon index, valeur renvoyée *)
			then value
		else
			regs_get_value_from_idx (List.tl regList) reg_idx
;;

(* Recupere la liste à partir d'un indice du registre *)

let rec regs_get_list_from_idx regList reg_id =
	if regList = [] (* si liste vite : erreur *)
		then failwith "failed"
	else
	match (List.hd regList) with
	|Reg(index, value) ->
		if (index = reg_id) (* si on est au bon index, liste à partir de la valeur renvoyée *)
			then regList (* <-- correction ici *)
		else
			regs_get_list_from_idx (List.tl regList) reg_id
;;

(* Parcours de liste avec une liste de ce qu'on a déjà regardé,
on modifie ce qu'il y a à reg_idx par ce qu'il y a à reg_idy*)

let copy_from_idy_to_idx regList reg_idx reg_idy =
	let rec aux l1 l2 lOriginal idx idy =
		match List.hd l1 with
		|Reg(index, value)->
			if(index = idx)
				then (List.rev l2)@((List.hd (regs_get_list_from_idx lOriginal idy))::(List.tl l1))
			else
				aux (List.tl l1) ((List.hd l1)::l2) lOriginal idx idy
	in aux regList [] regList reg_idx reg_idy
;;

(* Ajoute 1 à la valeur de la liste à l'index indiqué *)

let succ_reg_from_idx regList regidx =
	let rec aux l1 l2 =
		match List.hd l1 with
		|Reg(index, value)->
			if(index = regidx)
				then (List.rev l2)@(Reg(index, (value+1))::(List.tl l1))
			else
				aux (List.tl l1) ((List.hd l1)::l2)
	in aux regList []
;;

(* Change la valeur à l'index x *)

let change_reg_at_idx regList regid newValue =
	let rec aux l1 l2 idx =
		match List.hd l1 with
		|Reg(index, value)->
			if(index = regid)
				then (List.rev l2)@(Reg(index, newValue)::(List.tl l1))
			else
				aux (List.tl l1) ((List.hd l1)::l2) regid
	in aux regList [] regid
;;

(* Effectue une commande à un pointeur donné et renvoie le pointeur actuel dans la liste des instructions à faire *)

let run_cmd regList urm_cmd instptr =
	match urm_cmd with
	|Copy(regidx, regidy) -> [instptr_move_down instptr; (copy_from_idy_to_idx regList regidx regidy)]
	|Jump(regidx, regidy, line) ->
		begin match (reg_compar (List.hd (regs_get_list_from_idx regList regidx)) (List.hd (regs_get_list_from_idx regList regidy))) with
		|0 -> let rec jump_aux instpr_aux acc =
				if acc > 0
					then jump_aux (instptr_move_up instpr_aux) (acc-1)
				else if acc < 0
					then jump_aux (instptr_move_down instpr_aux) (acc +1)
				else [instpr_aux; regList]
			in jump_aux instptr ((get_instptr_line instptr)-line)
		|_-> failwith "failed"
		end
	|Succ(regidx) -> [instptr_move_down instptr; change_reg_at_idx regList regidx 0]
	|Zero(regidx) -> [instptr_move_down instptr; change_reg_at_idx regList regidx 0]
;;

(* Runs an URM.
 * Returns all registers when the program halts. *)
(* val urm_run : urm -> reg list = <fun> *)

(* let urm_run urm =
  let rec aux urm =
    match List.hd urm with
    |InstPtr(_,[])-> List.tl urm
    |_ -> aux (run_cmd)
;; *)

(* me marche pas, idées ?*)

(* let rec urm_run' urm =
	match List.hd urm with
	|InstPtr(_,[]) -> List.tl urm
	|tester chaque commande et utiliser les fonctions de gestion de registre dans chacune d'entre elle ? *)

(* Runs an URM in trace mode.
 * Returns all registers when the program halts. *)
(* val urm_run_trace : urm -> regval = <fun> *)

(* let urm_run_trace =

val urm_mk : urmcmd list -> reg list -> urm = <fun>

let urm_mk = *)
