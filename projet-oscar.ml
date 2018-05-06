


(* let state = (Label, Registre);; *)
(* prochain label utilisable et prochain registre utilisable *)

let getStateLabel state =
	match state with
	| State(label, reg) -> label
;;

let getStateRegister state =
	match state with
	| State(label, reg) -> reg
;;

let setNewStateRegister state newReg =
	match state with
	| State(label, reg) -> State(label, newReg)
;;

let setNewStateLabel state =
	match state with
	| State(label, reg) -> State(string_of_int ((int_of_string label) + 1), reg)
;;


(*déjà écrit dans projet-florian 
let rec compile_label_out is =
	let rec aux is acc =
		match is with (* is: instructions *)
		|[] -> []
		|Label s::is' -> ("Label " ^ acc) :: (aux is' (acc+1))
		|i::is' -> i::(aux is' acc)
	in aux is 1;
;;*)

(* Etape 1 *)

let dec_out (is, state)  =
	let rec aux is =
	    match is with
	    |[] -> []
	    |Dec(ri)::is' -> 
		    Zero(getStateRegister state)::
		    Inc(getStateRegister state)::
		    Sub(ri,getStateRegister state)::aux is' (*ri : registre index*)
	    |i::is' -> i::aux is'
	in (aux is, state)
;;

let mult_out (is, state) =
	let rec aux is = 
	    match is with
	    |[] -> []
	    |Mult(ri1,ri2)::is' -> 
			Label(getStateLabel state)::
			Zero(getStateRegister state)::
			Add(ri1, ri1)::
			Inc(getStateRegister state)::
			LTPredicate(getStateRegister state, ri2,  getStateLabel state):: (* si valeurs différentes, boucler *)
			aux is'
	    |i::is' -> i::aux is'
	in (aux is, setNewStateLabel state)
;;

let zero_predicate_out (is, state) = 
	let rec aux is = 
		match is with
		|[] -> []
		|ZeroPredicate(ri,label)::is' -> 
			Zero(getStateRegister state)::
			EqPredicate(ri, getStateRegister state, label)::
			aux is'
		|i::is' -> i::aux is'
	in (aux is, state)
;;

let geqpredicate_out (is, state) = 
	let rec aux is = 
		match is with
			|[] -> []
			|GEqPredicate(ri1,ri2,label)::is' -> 
				EqPredicate(ri1, ri2, label)::
				GTPredicate(ri1, ri2, label)::
				aux is'
			|i::is' -> i::aux is'
	in (aux is, state)
;;

let leqpredicate_out (is, state) = 
	let rec aux is = 
		match is with
		|[] -> []
		|LEqPredicate(ri1,ri2,label)::is' -> 
			EqPredicate(ri1, ri2, label)::
			GTPredicate(ri2, ri1, label)::
			aux is'
		|i::is' -> i::aux is'
	in (aux is, state)
;;

let ltpredicate_out (is, state) = 
	let rec aux is = 
		match is with
			|[] -> []
			|LTPredicate(ri1,ri2,label)::is' -> 
				GTPredicate(ri2, ri1, label)::
				aux is'
			|i::is' -> i::aux is'
	in (aux is, state)
;;

let compile_stage1 (is, state) = ltpredicate_out (leqpredicate_out (geqpredicate_out (zero_predicate_out (mult_out (dec_out (is,state))))));;


(* Etape 2 *)

let add_out (is, state) = 
	let rec aux is = 
	    match is with
	    |[] -> []
	    |Add(ri1,ri2)::is' -> 
			Label(getStateLabel state)::
			Zero(getStateRegister state)::
			Inc(ri1)::
			Inc(getStateRegister state)::
			GTPredicate(ri2, getStateRegister state, getStateLabel state)::
			aux is'
	    |i::is' -> i::aux is'
	in (aux is, setNewStateLabel state)
;;

let sub_out is = 
	let rec aux is = 
		match is with
		|[] -> []
		|Sub(ri1,ri2)::is' -> Add(0,(ri1-ri2)::sub_out is'
		|i::is' -> i::sub_out is'
	in (aux is, setNewStateLabel state)
;;

(* à enlever

let rec gtpredicate_out is = 
	match is with
	|[] -> []
	|GTqPredicate(ri1, ri2, label)::is' -> ??????::gtpredicate_out is' ???????
	|i::is' -> i::gtpredicate_out is'
;;

let compile_stage2 is = gtpredicate_out( sub_out( add_out(is) ) );;

(* Etape 3 *)

let rec goto_out is = is ;;

let compile_stage3 is = goto_out is;;

(* Etape 4 *)

let rec inc_out is = is ;;

let rec eqpredicate is = is ;;

let rec sub is = is ;;

let compile_stage4 is = inc_out (eqpredicate (sub is) );;


















*)