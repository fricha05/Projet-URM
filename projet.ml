



(* Florian RICHARD et Oscar FALMER *)




#use "types.ml";;
#load "str.cma";;

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
;;

(* ( [], [(0, URMZero O), (1, URMJump 1 3 6), ...] ) *)

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
	| (_, URMCopy(i,j)) -> "URMCopy " ^ string_of_int i
	| (_, URMJump(i, j, k)) -> "URMJump " ^ string_of_int(i) ^ " " ^ string_of_int(j) ^ " " ^ string_of_int(k)
	| (_, URMSucc(i)) -> "URMSucc"
	| (_, URMZero(i)) -> "URMZero"
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
 * It returns an integer less than, equal to, or greater than URMZero if
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
		then failwith "failed - regs_get_value_from_idx"
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
		then failwith "failed - regs_get_list_from_idx"
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

let URMCopy_from_idy_to_idx regList reg_idx reg_idy =
	let rec aux l1 l2 lOriginal idx idy =
		if l1 = []
			then failwith "failed - URMCopy_from_idy_to_idx"
		else
			match List.hd l1 with
			|Reg(index, value)->
				if(index = idx)
					then (List.rev l2)@((List.hd (regs_get_list_from_idx lOriginal idy))::(List.tl l1))
				else
					aux (List.tl l1) ((List.hd l1)::l2) lOriginal idx idy
	in aux regList [] regList reg_idx reg_idy
;;

(* Ajoute 1 à la valeur de la liste à l'index indiqué *)

let URMSucc_reg_from_idx regList regidx =
	let rec aux l1 l2 =
		if l1 = []
			then failwith "failed - URMSucc_reg_from_idx"
		else
			match List.hd l1 with
			|Reg(index, value)->
				if(index = regidx)
					then (List.rev l2)@(Reg(index, (value+1))::(List.tl l1))
				else if (regidx < index)
					then (List.rev l2)@(Reg(regidx, (value+1))::l1)
				else if (List.tl l1 == [])
					then (List.rev l2)@l1@[Reg(regidx, (value+1))]
				else
					aux (List.tl l1) ((List.hd l1)::l2)
	in aux regList []
;;

(* Returns Regs with String format *)

let rec regs_get_string regList =
	if regList = []
		then ""
	else
		match (List.hd regList) with
		|Reg(index, value) -> if (List.tl regList) == []
									then "(" ^ (string_of_int index) ^ "," ^ (string_of_int value) ^ ")" ^ regs_get_string (List.tl regList)
								else
									"(" ^ (string_of_int index) ^ "," ^ (string_of_int value) ^ ")," ^ regs_get_string (List.tl regList)
;;

(* Change la valeur à l'index x *)

let change_reg_at_idx regList regidx newValue =
	let rec aux l1 l2 =
		if l1 = []
			then failwith "failed - change_reg_at_idx"
		else
			match List.hd l1 with
			|Reg(index, value)->
				if(index = regidx)
					then (List.rev l2)@(Reg(index, newValue)::(List.tl l1))
				else if (regidx < index)
					then (List.rev l2)@(Reg(regidx, newValue)::l1)
				else if (List.tl l1 == [])
					then (List.rev l2)@l1@[Reg(regidx, newValue)]
				else
					aux (List.tl l1) ((List.hd l1)::l2)
	in aux regList []
;;

(* Récupère prochaine commande à partir d'un inst_ptr *)

let instptr_get_cmd inst_ptr =
	match (instptr_get inst_ptr) with
	| (_, cmd) -> cmd
;;

(* Effectue une commande à un pointeur donné et renvoie le pointeur actuel dans la liste des instructions à faire *)

(* Runs an URM.
 * Returns all registers when the program halts. *)
(* val urm_run : urm -> reg list = <fun> *)

let run_cmd_aux regList instptr =
	match (instptr_get_cmd instptr) with
	|URMCopy(regidx, regidy) -> {instptr = (instptr_move_down instptr); regs = (URMCopy_from_idy_to_idx regList regidx regidy)}
	|URMJump(regidx, regidy, line) ->
		begin match (reg_compar (List.hd (regs_get_list_from_idx regList regidx)) (List.hd (regs_get_list_from_idx regList regidy))) with
		|0 -> let rec URMJump_aux instpr_aux acc =
				if acc > 0
					then URMJump_aux (instptr_move_up instpr_aux) (acc-1)
				else if acc < 0
					then URMJump_aux (instptr_move_down instpr_aux) (acc+1)
				else {instptr = instpr_aux; regs = regList}
			in URMJump_aux instptr ((get_instptr_line instptr)-line)
    |1 -> {instptr = (instptr_move_down instptr); regs = regList}
		|_-> failwith "failed - run_cmd_aux"
		end
	|URMSucc(regidx) -> {instptr = (instptr_move_down instptr); regs = (URMSucc_reg_from_idx regList regidx)}
	|URMZero(regidx) -> {instptr = (instptr_move_down instptr); regs = (change_reg_at_idx regList regidx 0)}
;;

let run_cmd urmrecue =
	match urmrecue with
	| {instptr; regs} -> (run_cmd_aux regs instptr)
;;

(* Récupère la liste des registres à partir d'un urm. *)

let get_reglist_from_urm urmrecue =
	match urmrecue with
	| {instptr; regs} -> regs
;;

(* Récupère instptr à partir d'un urm. *)

let get_instptr_from_urm urmrecue =
	match urmrecue with
	| {instptr; regs} -> instptr
;;

(* urm_run *)

let rec urm_run urm =
  match get_instptr_from_urm urm with
    |InstPtr(_,[])-> (get_reglist_from_urm urm)
    |_ -> urm_run (run_cmd urm)
;;


(* Runs an URM in trace mode.
 * Returns all registers when the program halts. *)
(* val urm_run_trace : urm -> regval = <fun> *)

let run_cmd_aux_bis regList instptr =
	match (instptr_get_cmd instptr) with
	|URMCopy(regidx, regidy) -> (Printf.printf "%d: URMCopy %d %d\n%s \n\n" (get_instptr_line instptr) regidx regidy (regs_get_string regList); {instptr = (instptr_move_down instptr); regs = (URMCopy_from_idy_to_idx regList regidx regidy)})
	|URMJump(regidx, regidy, line) ->
		begin match (reg_compar (List.hd (regs_get_list_from_idx regList regidx)) (List.hd (regs_get_list_from_idx regList regidy))) with
		|0 -> let rec URMJump_aux instpr_aux acc =
				if acc > 0
					then URMJump_aux (instptr_move_up instpr_aux) (acc-1)
				else if acc < 0
					then URMJump_aux (instptr_move_down instpr_aux) (acc+1)
				else (Printf.printf "%d: URMJump %d %d %d\n%s \n\n" (get_instptr_line instptr) regidx regidy line (regs_get_string regList); {instptr = instpr_aux; regs = regList})
			in URMJump_aux instptr ((get_instptr_line instptr)-line)
    	|1 -> Printf.printf "%d: URMJump %d %d %d\n%s \n\n" (get_instptr_line instptr) regidx regidy line (regs_get_string regList); {instptr = (instptr_move_down instptr); regs = regList}
		|_-> failwith "failed - run_cmd_aux_bis"
		end
	|URMSucc(regidx) -> (Printf.printf "%d: URMSucc %d\n%s \n\n" (get_instptr_line instptr) regidx (regs_get_string regList);{instptr = (instptr_move_down instptr); regs = (URMSucc_reg_from_idx regList regidx)})
	|URMZero(regidx) -> (Printf.printf "%d: URMZero %d\n%s \n\n" (get_instptr_line instptr) regidx (regs_get_string regList);{instptr = (instptr_move_down instptr); regs = (change_reg_at_idx regList regidx 0)})
;;

let run_cmd_bis urmrecue =
	match urmrecue with
	| {instptr; regs} -> (run_cmd_aux_bis regs instptr)
;;

let urm_run_trace urm =
    let rec aux urm =
        match get_instptr_from_urm urm with
    	    |InstPtr(_,[])-> (get_reglist_from_urm urm)
    	    |_ -> aux (run_cmd_bis urm)
   	in aux urm
;;


(* createLineUrmcmdList - Créer la liste de tuples avec une commande et le numéro d'index correspondant *)

let createLineUrmcmdList urmcmdlist =
	let rec aux list acc =
		match list with
		| [] -> []
		| _ -> [(acc, (List.hd list))] @ aux (List.tl list) (acc+1)
	in aux urmcmdlist 0
;;

(*val urm_mk : urmcmd list -> reg list -> urm = <fun>*)

let urm_mk urmcmdlist reglist = {instptr = InstPtr([], (createLineUrmcmdList urmcmdlist) ); regs = reglist};;


(** Tests **)

let m = urm_mk add_program [Reg (1, 2); Reg (2, 3)];;

urm_run m;;

urm_run_trace m;;

(*
  gestion des labels :
  ls = [...]
  [string] -> [int] -> [(string, int)]
  ["toto";"tata";"titi"]
  [("toto",1);("tata";2);("titi",3)]
*)
