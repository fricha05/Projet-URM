



(*

	Projet - Machine a registres non limites
	Etudiants - Florian RICHARD et Oscar FALMER
	Fichier - Partie 2 EURM

--Informations générales--

labels commencent à 1
State("1",1) label en string en premier, state montrent le premier index de registre dispo à utiliser et pareil pour le label
*)


(* fst : premier élément d'un tuple, snd : second élément d'un tuple *)
(* OK *)

let rec print_list = function 
[] -> (Printf.printf ("\n\n"))
| (label, nb)::l -> Printf.printf "(label : %s, nb : %d)" label nb ; print_string " " ; print_list l

let rec label_to_line label listeLabels =
	if listeLabels = []
		then failwith "Erreur label_to_line"
	else if fst (List.hd listeLabels) = label
		then (Printf.printf "Label trouve !! \n\n " ; snd (List.hd listeLabels))
	else
		(Printf.printf "Label %s (à trouver %s) - " (fst (List.hd listeLabels)) label ; label_to_line label (List.tl listeLabels))

let rec compile_comment_out is =
    match is with
    |[] -> []
    |Comment s::is' -> compile_comment_out is'
    |i::is' -> i::compile_comment_out is'
;;

let rec compile_label_out is  =
	let rec aux is isFinal acc listeLabels =
		match is with 
		|[] -> (Printf.printf "Lecture de la Liste des Labels en compile label out : "; print_list listeLabels ;auxBis isFinal listeLabels)
		|Label l::is' -> aux is' (isFinal@[Label (string_of_int acc)]) (acc+1) ((l,acc)::listeLabels)
		|i::is' -> aux is' (isFinal@[i]) acc listeLabels
	and auxBis isBis listeLabels =
		match isBis with
		| [] -> []
		| EqPredicate(r1,r2,label)::is' -> EqPredicate(r1,r2, string_of_int (label_to_line label listeLabels))::(auxBis is' listeLabels)
		| GTPredicate(r1,r2,label)::is' -> GTPredicate(r1,r2, string_of_int (label_to_line label listeLabels))::(auxBis is' listeLabels)
		| LTPredicate(r1,r2,label)::is' -> LTPredicate(r1,r2, string_of_int (label_to_line label listeLabels))::(auxBis is' listeLabels)
		| LEqPredicate(r1,r2,label)::is' -> LEqPredicate(r1,r2, string_of_int (label_to_line label listeLabels))::(auxBis is' listeLabels)
		| GEqPredicate(r1,r2,label)::is' -> GEqPredicate(r1,r2, string_of_int (label_to_line label listeLabels))::(auxBis is' listeLabels)
		| ZeroPredicate(r1,label)::is' -> ZeroPredicate(r1, string_of_int (label_to_line label listeLabels))::(auxBis is' listeLabels)
		| Goto(label)::is' -> Goto(string_of_int (label_to_line label listeLabels))::(auxBis is' listeLabels)
		| q::is' -> q::(auxBis is' listeLabels)
	in aux is [] 1 [];
;;

let rec create_state_label is =
	let rec aux is acc =
		match is with
		|[] -> (Printf.printf "Prochain label disponible %i \n" acc; string_of_int acc)
		|Label s::is' -> aux is' (acc+1)
		|i::is' -> (aux is' acc)
	in aux is 1;
;;

let max a b = if (a > b) then a else b;;
let max_ter a b c = max (max a b) c ;;

let rec create_state_reg is =
	let rec aux is regidmax =
		match is with
		|[] -> (Printf.printf "RegMax : %d \n\n" regidmax; (regidmax+1))
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

let getStateLabelPlus state nb =
	match state with
	| State(label, reg) -> (string_of_int ((int_of_string label)+nb))
;;

let getStateRegister state =
	match state with
	| State(label, reg) -> reg
;;

let getStateRegisterPlus state nb =
	match state with
	| State(label, reg) -> (reg+nb)
;;

let setNewStateRegister state =
	match state with
	| State(label, reg) -> State(label, (reg+1))
;;

let setNewStateRegisterPlus state nb =
	match state with
	| State(label, reg) -> State(label, (reg+nb))
;;

let setNewStateLabel state =
	match state with
	| State(label, reg) -> State(string_of_int ((int_of_string label) + 1), reg)
;;

let setNewStateLabelPlus state nb =
	match state with
	| State(label, reg) -> State(string_of_int ((int_of_string label) + nb), reg)
;;

(* Etape 1 *)

(* OK *)
let dec_out (is, state)  =
	let rec aux is finalIs state =
	    match is with
	    |[] -> (finalIs, state)
	    |Dec(ri)::is' ->
	    	aux is' (finalIs@(
		    Zero(getStateRegisterPlus state 0)::
		    Inc(getStateRegisterPlus state 0)::
		    Sub(ri,getStateRegisterPlus state 0)::
		    [])) (setNewStateRegisterPlus state 1)
	    |i::is' -> aux is' (finalIs@[i]) state
	in aux is [] state
;;

let mult_out (is, state) =
	let rec aux is finalIs state =
	    match is with
	    |[] -> (finalIs, state)
	    |Mult(ri1,ri2)::is' ->
	    	aux is' (finalIs@(

	    	(*Cas multiplication 0 et 1*)
	    	ZeroPredicate(ri2, getStateLabelPlus state 2)::
	    	Zero(getStateRegisterPlus state 2)::
	    	Inc(getStateRegisterPlus state 2)::
	    	EqPredicate(ri2, getStateRegisterPlus state 2, getStateLabelPlus state 3)::

	    	(*Sauvegarde valeur ri1*)
	    	Zero(getStateRegisterPlus state 0)::
	    	Label(getStateLabelPlus state 0)::
	    	Inc(getStateRegisterPlus state 0)::
	    	LTPredicate(ri1, getStateRegisterPlus state 0, getStateLabelPlus state 0):: (*inverse chelou*)

	    	Zero(getStateRegisterPlus state 1)::
	    	Inc(getStateRegisterPlus state 1)::
			Label(getStateLabelPlus state 1)::
			Add(ri1, getStateRegisterPlus state 0)::
			Inc(getStateRegisterPlus state 1)::
			EqPredicate(getStateRegisterPlus state 1, ri2,  getStateLabelPlus state 3):: (* si valeurs différentes, boucler *)
			Goto(getStateLabelPlus state 1)::

			Label(getStateLabelPlus state 2)::
			Zero(ri1)::

			Label(getStateLabelPlus state 3)::

			[])) (setNewStateLabelPlus (setNewStateRegisterPlus state 3) 4)
	    |i::is' -> aux is' (finalIs@[i]) state
	in aux is [] state
;;

let zero_predicate_out (is, state) =
	let rec aux is finalIs state =
		match is with
		|[] -> (finalIs, state)
		|ZeroPredicate(ri,label)::is' ->
			aux is' (finalIs@(
			Zero(getStateRegisterPlus state 0)::
			EqPredicate(ri, getStateRegisterPlus state 0, label)::
			[])) (setNewStateRegisterPlus state 1)
		|i::is' -> aux is' (finalIs@[i]) state
	in aux is [] state
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
	let rec aux is finalIs state =
	    match is with
	    |[] -> (finalIs, state)
	    |Add(ri1,ri2)::is' ->
	    	aux is' (finalIs@(
	    	Zero(getStateRegisterPlus state 0)::
			EqPredicate(ri2, getStateRegisterPlus state 0, getStateLabelPlus state 1)::

			Label(getStateLabelPlus state 0)::
			Inc(ri1)::
			Inc(getStateRegisterPlus state 0)::
			GTPredicate(getStateRegisterPlus state 0, ri2, getStateLabelPlus state 0):: (*(inversé ?)*)

			Label(getStateLabelPlus state 1)::
			[])) (setNewStateLabelPlus (setNewStateRegisterPlus state 1) 2 )
	    |i::is' -> aux is' (finalIs@[i]) state
	in aux is [] state
;;

let sub_out (is, state) =
	let rec aux is finalIs state =
		match is with
			|[] -> (finalIs, state)
			|Sub(ri1,ri2)::is' -> (* A - B *)

				aux is' (finalIs@(
				(* A : valeur tampon égale à r1 *)
				Zero(getStateRegisterPlus state 0)::
				Label(getStateLabelPlus state 0)::
				Inc(getStateRegisterPlus state 0)::
				GTPredicate(getStateRegisterPlus state 0, ri1, getStateLabelPlus state 0):: (*inverse chelou*)

				(* B : valeur tampon égale à r2 *)
				Zero(getStateRegisterPlus state 1)::
				Label(getStateLabelPlus state 1)::
				Inc(getStateRegisterPlus state 1)::
				GTPredicate(getStateRegisterPlus state 1,ri2, getStateLabelPlus state 1):: (*inverse chelou*)

				Zero(ri1)::
				Label(getStateLabelPlus state 2)::
				Inc(ri1)::
				Inc(getStateRegisterPlus state 1):: (* Incrémente B *)
				GTPredicate(getStateRegisterPlus state 1, getStateRegisterPlus state 0, getStateLabelPlus state 2):: (* On compare A et B *) (*inverse chelou*)

				[])) (setNewStateLabelPlus (setNewStateRegisterPlus state 2) 3 )
			|i::is' -> aux is' (finalIs@[i]) state
	in aux is [] state
;;

let gtpredicate_out (is, state) =
	let rec aux is finalIs state =
		match is with
		|[] -> (finalIs,state)
		|GTPredicate(ri1, ri2, label)::is' ->
			aux is' (finalIs@(
			(* si égal, saute *)
			EqPredicate(ri1, ri2, getStateLabelPlus state 1)::
			(* sinon *)

			(* A : valeur tampon *)
			Zero(getStateRegisterPlus state 0)::

			Label(getStateLabelPlus state 0)::
			Inc(getStateRegisterPlus state 0)::

			EqPredicate(getStateRegisterPlus state 0, ri1, label):: (*inverse chelou*)
			EqPredicate(getStateRegisterPlus state 0, ri2, getStateLabelPlus state 1):: (*inverse chelou*)

			Goto(getStateLabelPlus state 0)::

			Label(getStateLabelPlus state 1)::[])) (setNewStateRegisterPlus (setNewStateLabelPlus state 2) 1)
		|i::is' -> aux is' (finalIs@[i]) state
	in aux is [] state
;;

let compile_stage2 (is, state) = gtpredicate_out( sub_out( add_out((is,state)) ) );;

(* Etape 3 *)

let goto_out (is, state) =
	let rec aux is finalIs state =
		match is with
		|[] -> (finalIs, state)
		|Goto(label)::is' ->
			aux is' (finalIs@(
			Zero(getStateRegisterPlus state 0)::
			EqPredicate(getStateRegisterPlus state 0, getStateRegisterPlus state 0, label)::
			[])) (setNewStateRegisterPlus state 1)
		|i::is' -> aux is' (finalIs@[i]) state
	in aux is [] state
;;

let compile_stage3 (is, state) = goto_out (is, state);;

(* Etape 4 *)

(*
premiere ligne : index 0
création d'une liste de tuples d'index et de la ligne suivante correspondante
*)

let compile_stage4 (is, state) =
	let rec suppression_liste is isFinal listeLabels acc =
		match is with
		|[] -> (Printf.printf "Lecture de la Liste des Labels en stage 4 : "; print_list listeLabels ;aux isFinal listeLabels)
		|Label l::is' -> suppression_liste is' isFinal ((l, acc)::listeLabels) acc
		|q::is' -> suppression_liste is' (isFinal@[q]) listeLabels (acc+1)
	and aux is listeLabels =
		match is with
		|[] -> []
		|Inc r1::is' -> URMSucc(r1)::aux is' listeLabels
		|Zero r1::is' -> URMZero(r1)::aux is' listeLabels
		|EqPredicate(r1,r2,label)::is' -> (print_list listeLabels; Printf.printf "Label à trouver %s - " label ; URMJump(r1, r2, (label_to_line label listeLabels))::aux is' listeLabels)
		|Copy(r1,r2)::is' -> URMCopy(r1,r2)::aux is' listeLabels
		|_::is' -> aux is' listeLabels
	in suppression_liste is [] [] 0
;;

(* Compilation totale *)

let urm_from_eurm eurmcmdList = compile_stage4 (compile_stage3 (compile_stage2 (compile_stage1 ( compile_preprocess(eurmcmdList) ) ) ) );;





