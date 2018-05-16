



(*

	Projet - Machine a registres non limites
	Etudiants - Florian RICHARD et Oscar FALMER
	Fichier - Partie 2 EURM

--Informations générales--

labels commencent à 1
State("1",1) label en string en premier, state montrent le premier index de registre dispo à utiliser et pareil pour le label

VERIFIER LE SENS DE TOUTES LES COMPARAISONS

*)




let rec compile_comment_out is =
    match is with
    |[] -> []
    |Comment s::is' -> compile_comment_out is'
    |i::is' -> i::compile_comment_out is'
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
	let rec aux is regidmax =
		match is with
		|[] -> (regidmax+1)
		|Add(r1,r2)::is' -> aux is' (max_ter r1 r2 regidmax)
		|Copy(r1,r2)::is' -> aux is' (max_ter r1 r2 regidmax)
		|Dec(r1)::is' -> aux is' (max r1 regidmax)
		|EqPredicate(r1,r2,l)::is' -> aux is' (max_ter r1 r2 regidmax)
		|GEqPredicate(r1,r2,l)::is' -> aux is' (max_ter r1 r2 regidmax)
		|GTPredicate(r1,r2,l)::is' -> aux is' (max_ter r1 r2 regidmax)
		|Inc r1::is' -> aux is' (max r1 regidmax)
		|LEqPredicate(r1,r2,l)::is' -> aux is' (max_ter r1 r2 regidmax)
		|LTPredicate(r1,r2,l)::is' -> aux is' (max_ter r1 r2 regidmax)
		|Mult(r1,r2)::is' -> aux is' (max_ter r1 r2 regidmax)
		|Sub(r1,r2)::is' -> aux is' (max_ter r1 r2 regidmax)
		|Zero r1::is' -> aux is' (max r1 regidmax)
		|ZeroPredicate(r1,l)::is' -> aux is' (max r1 regidmax)
		|i::is' -> ( aux is' regidmax)
	in aux is 0 (* par défaut : commence à 0 *)
;;

let create_state is = (is, State(create_state_label is, create_state_reg is));;

(* ajoute le registre maximum + 1 au state
let compile_state_register *)

(* nombre de registres maximum + 1 : state *)

let compile_preprocess is = create_state (compile_label_out(compile_comment_out is));;

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

(* Etape 1 *)

(* OK *)
let dec_out (is, state)  =
	let rec aux is =
	    match is with
	    |[] -> []
	    |Dec(ri)::is' ->
		    Zero(getStateRegister state)::
		    Inc(getStateRegister state)::
		    Sub(ri,getStateRegister state)::
		    aux is' (*ri : registre index*)
	    |i::is' -> i::aux is'
	in (aux is, setNewStateRegister state)
;;

(* OK, corrigé boucle infinie *)
let mult_out (is, state) =
	let rec aux is =
	    match is with
	    |[] -> []
	    |Mult(ri1,ri2)::is' ->
	    	Zero(getStateRegister state)::
			Label(getStateLabel state)::
			Add(ri1, ri1)::
			Inc(getStateRegister state)::
			LTPredicate(getStateRegister state, ri2,  getStateLabel state):: (* si valeurs différentes, boucler *)
			aux is'
	    |i::is' -> i::aux is'
	in (aux is, setNewStateRegister (setNewStateLabel state))
;;

(* OK *)
let zero_predicate_out (is, state) =
	let rec aux is =
		match is with
		|[] -> []
		|ZeroPredicate(ri,label)::is' ->
			Zero(getStateRegister state)::
			EqPredicate(ri, getStateRegister state, label)::
			aux is'
		|i::is' -> i::aux is'
	in (aux is, setNewStateRegister state)
;;

(* OK *)
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

(* OK *)
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

(* OK *)
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

(* OK *)
let compile_stage1 (is, state) = ltpredicate_out (leqpredicate_out (geqpredicate_out (zero_predicate_out (mult_out (dec_out (is,state))))));;


(* Etape 2 *)

(* OK *)
let add_out (is, state) =
	let rec aux is =
	    match is with
	    |[] -> []
	    |Add(ri1,ri2)::is' ->
	    	Zero(getStateRegister state)::
			Label(getStateLabel state)::
			Inc(ri1)::
			Inc(getStateRegister state)::
			GTPredicate(ri2, getStateRegister state, getStateLabel state)::
			aux is'
	    |i::is' -> i::aux is'
	in (aux is, setNewStateRegister (setNewStateLabel state))
;;

(* OK *)
let sub_out (is, state) =
	let rec aux is =
		match is with
			|[] -> []
			|Sub(ri1,ri2)::is' -> (* A - B *)

				(* A : valeur tampon égale à r1 *)
				Zero(getStateRegister state)::
				Label(getStateLabel state)::
				Inc(getStateRegister state)::
				GTPredicate(ri1, getStateRegister state, getStateLabel state)::

				(* B : valeur tampon égale à r2 *)
				Zero(getStateRegister (setNewStateRegister state))::
				Label(getStateLabel (setNewStateLabel state))::
				Inc((getStateRegister (setNewStateRegister state)))::
				GTPredicate(ri2, getStateRegister (setNewStateRegister state), getStateLabel (setNewStateLabel state))::

				Zero(ri1)::
				Label(getStateLabel (setNewStateLabel (setNewStateLabel state)))::
				Inc(ri1)::
				Inc(getStateRegister (setNewStateRegister state)):: (* Incrémente B *)
				GTPredicate(getStateRegister state, getStateRegister (setNewStateRegister state), getStateLabel (setNewStateLabel (setNewStateLabel state))):: (* On compare A et B *)

				aux is'
			|i::is' -> i::aux is'
	in (aux is, setNewStateRegister (setNewStateRegister (setNewStateLabel (setNewStateLabel (setNewStateLabel state)))))
;;

(* OK *)
let gtpredicate_out (is, state) =
	let rec aux is =
		match is with
		|[] -> []
		|GTPredicate(ri1, ri2, label)::is' ->

			(* si égal, saute *)
			EqPredicate(ri1, ri2, getStateLabel (setNewStateLabel state))::
			(* sinon *)

			(* A : valeur tampon *)
			Zero(getStateRegister state)::

			Label(getStateLabel state)::
			Inc(getStateRegister state)::

			EqPredicate(getStateRegister state, ri1, label)::
			EqPredicate(getStateRegister state, ri2, getStateLabel (setNewStateLabel state))::

			Goto(getStateLabel state)::

			Label(getStateLabel (setNewStateLabel state))::
			aux is'
		|i::is' -> i::aux is'
	in (aux is, setNewStateRegister (setNewStateLabel (setNewStateLabel state)))
;;

(* OK *)
let compile_stage2 (is, state) = gtpredicate_out( sub_out( add_out((is,state)) ) );;

(* Etape 3 *)

(*let rec code_a_partir_du_label is label =
	match is with
	|[] -> []
	|Label l::is' -> if (l = label) then is'
					else code_a_partir_du_label is' label
	|i::is' -> code_a_partir_du_label is' label
;;*)

(* OK *)
let goto_out (is, state) =
	let rec aux is =
		match is with
		|[] -> []
		|Goto(label)::is' ->
			Zero(getStateRegister state)::
			EqPredicate(getStateRegister state, getStateRegister state, label)::
			aux is'
		|i::is' -> i::aux is'
	in (aux is, setNewStateRegister state)
;;

(* OK *)
let compile_stage3 (is, state) = goto_out (is, state);;

(* Etape 4 *)

(*let label_to_line is label =
	let rec aux is acc =
		match is with
		|[] -> 0
		|Label l::is' -> if (l = label) then acc  (* à tester *)
						else aux is' (acc) (* ne compte pas les labels *)
		|i::is' -> aux is' (acc+1)
	in aux is 0
;;*)

(* fst : premier élément d'un tuple, snd : second élément d'un tuple *)
(* OK *)
let rec label_to_line label listeLabels =
	if fst (List.hd listeLabels) = label
		then snd (List.hd listeLabels)
	else
		label_to_line label (List.tl listeLabels)

(*
premiere ligne : index 0
création d'une liste de tuples d'index et de la ligne suivante correspondante
*)

(* OK *)
let compile_stage4 (is, state) =
	let rec suppression_liste is isFinal listeLabels acc =
		match is with
		|[] -> aux isFinal listeLabels
		|Label l::is' -> suppression_liste is' isFinal ((l, acc)::listeLabels) acc
		|q::is' -> suppression_liste is' (isFinal@[q]) listeLabels (acc+1)
	and aux is listeLabels =
		match is with
		|[] -> []
		|Inc r1::is' -> URMSucc(r1)::aux is' listeLabels
		|Zero r1::is' -> URMZero(r1)::aux is' listeLabels
		|EqPredicate(r1,r2,label)::is' -> URMJump(r1, r2, (label_to_line label listeLabels))::aux is' listeLabels
		|Copy(r1,r2)::is' -> URMCopy(r1,r2)::aux is' listeLabels
		|_::is' -> aux is' listeLabels
		(*|i::is' -> i::aux is'*)
	in suppression_liste is [] [] 0 (*state state à garder ?? *)
;;

(* Compilation totale *)

let urm_from_eurm eurmcmdList = compile_stage4 (compile_stage3 (compile_stage2 (compile_stage1 (eurmcmdList, State("0", 0)) ) ) );;
