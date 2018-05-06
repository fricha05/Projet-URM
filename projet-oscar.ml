


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

let setNewStateRegister state =
	match state with
	| State(label, reg) -> State(label, (reg+1))
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

let sub_out (is, state) = 
	let rec aux is = 
		match is with
			|[] -> []
			|Sub(ri1,ri2)::is' -> 

				(* A : valeur tampon égale à r2 *)
				Label(getStateLabel state)::
				Zero(getStateRegister state)::
				GTPredicate(getStateRegister state, ri2, getStateLabel state)::

				(* B : valeur tampon égale à r1 *)
				Label(getStateLabel (setNewStateLabel state))::
				Zero(getStateRegister (setNewStateRegister state))::
				GTPredicate(getStateRegister (setNewStateRegister state), ri2, getStateLabel (setNewStateLabel state))::

				Zero(ri1)::

				Label(getStateLabel (setNewStateLabel (setNewStateLabel state)))::
				Inc(ri1)::
				Inc(getStateRegister state):: (* Incrémente A *)
				GTPredicate(getStateRegister (setNewStateRegister state), getStateRegister state, getStateLabel (setNewStateLabel (setNewStateLabel state))):: (* On compare A et B *)

				aux is'
			|i::is' -> i::aux is'
	in (aux is, setNewStateLabel (setNewStateLabel (setNewStateLabel state)))
;;

let gtpredicate_out (is, state) = 
	let rec aux is = 
		match is with
		|[] -> []
		|GTPredicate(ri1, ri2, label)::is' -> 

			EqPredicate(ri1, ri2, getStateLabel (setNewStateLabel state))::

			(* A : valeur tampon *)
			Zero(getStateRegister state)::

			Label(getStateLabel state)::
			Inc(getStateRegister state)::
			
			EqPredicate(getStateRegister state, ri2, label)::
			EqPredicate(getStateRegister state, ri1, getStateLabel (setNewStateLabel state))::

			Goto(getStateLabel state)::

			Label(getStateLabel (setNewStateLabel state))::
			aux is'
		|i::is' -> i::aux is'
	in (aux is, setNewStateLabel (setNewStateLabel state))
;;

let compile_stage2 (is, state) = gtpredicate_out( sub_out( add_out((is,state)) ) );;

(* Etape 3 *)

let rec code_a_partir_du_label is label =
	match is with
	|[] -> []
	|Label l::is' -> if (l = label) then is'
					else code_a_partir_du_label is' label
	|i::is' -> code_a_partir_du_label is' label
;;

(** à vérifier, Goto me parait pas correct **)

let goto_out (is, state) = 
	let rec aux is =
		match is with
		|[] -> []
		|Goto(label)::is' -> aux (code_a_partir_du_label is' label)
		|i::is' -> i::aux is'
	in (aux is, setNewStateLabel (setNewStateLabel state))
;;

let compile_stage3 (is, state) = goto_out (is, state);;

(* Etape 4 *)

let label_to_line is label =
	let rec aux is acc =
		match is with
		|[] -> 0
		|Label l::is' -> if (l = label) then acc (* à tester *)
						else aux is' (acc) (* ne compte pas les labels *)
		|i::is' -> aux is' (acc+1)
	in aux is 1
;;

let compile_stage4 (is, state) = 
	let rec aux is =
		match is with
		|[] -> []
		|Inc r1::is' -> URMSucc(r1)::aux is'
		|Zero r1::is' -> URMZero(r1)::aux is'
		|EqPredicate(r1,r2,label)::is' -> URMJump(r1, r2, (label_to_line is label))::aux is'
		|Label l::is' -> aux is'
		|Copy(r1,r2)::is' -> URMCopy(r1,r2)::aux is'
		|_::is' -> aux is'
		(*|i::is' -> i::aux is'*)
	in (aux is, state) (* state à modifier ?? *)
;;

(* à enlever

let rec eqpredicate is = is ;;
let rec sub is = is ;;
let compile_stage4 is = inc_out (eqpredicate (sub is) );;


















*)