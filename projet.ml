
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

let rec regs_get regList reg_idx =
	if regList = []
		then failwith "failed"
	else
	match (List.hd regList) with
	| Reg(index, value) -> 
		if index = reg_idx
			then value
		else 
			regs_get (List.tl regList) reg_idx
;;

let rec regs_get_from_id regList reg_id =
	if regList = []
		then failwith "failed"
	else
	match (List.hd regList) with
	|Reg(index, value) -> 
		if (index = reg_id)
			then (List.hd regList)
		else
			regs_get_from_id (List.tl regList) reg_id
;;

let rec copy_from_id regList1 regList2 reg_idx reg_idy =
	match List.hd regList1 with
	|Reg(index, value)->
		if(index = reg_idx)
			then (List.rev regList2)@((regs_get_from_id regList1 reg_idy)::(List.tl regList1))
		else
			copy_from_id (List.tl regList1) ((List.hd regList1)::regList2) reg_idx reg_idy
;;

let rec succ_reg_from_id regList1 regList2 regid =
	match List.hd regList1 with
	|Reg(index, value)->
		if(index = regid)
			then (List.rev regList2)@(Reg(index, (value+1))::(List.tl regList1))
		else
			succ_reg_from_id (List.tl regList1) ((List.hd regList1)::regList2) regid
;;

let register_cmd regList urmcmd instptr =
	match urmcmd with
	|Copy(regidx, regidy) -> ((copy_from_id regList [] regidx regidy), instptr)
	|Jump(regidx, regidy, line) -> 
		begin match (reg_compar (regs_get_from_id regList regidx)(regs_get_from_id regList regidy)) with
		|0 -> let rec jump_aux instpr_aux acc =
				if acc > 0 
					then jump_aux (instptr_move_up instpr_aux) (acc-1)
				else if acc < 0
					then jump_aux (instptr_move_down instpr_aux) (acc +1)
				else (regList, instpr_aux)
			in jump_aux instptr ((get_instptr_line instptr)-line)
		|_-> failwith "failed"
		end
	|Succ(regidx) -> (succ_reg_from_id regList [] regidx, instptr)
	(*|Zero(regidx) -> *)
	|_-> failwith "failed"
;;


(* autres fonctions pour les registres à ajouter...
ajouter les fonctions Copy Jump Succ Zero pour pouvoir les réutiliser dans le run ?
*)

(* Runs an URM.
 * Returns all registers when the program halts. *)
(* val urm_run : urm -> reg list = <fun> *)

(* let urm_run urm =
	match urm with
	| (_, Reg list (regs) ) -> regs
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























