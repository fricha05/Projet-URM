



(*

	Projet - Machine a registres non limites
	Etudiants - Florian RICHARD et Oscar FALMER
	Fichier - Projet

*)




#use "types.ml";;
#use "partie1_urm.ml";;
#use "partie2_eurm.ml";;
#load "str.cma";;

(** Tests Rendu 1 **)

(*let m = urm_mk add_program [Reg (1, 2); Reg (2, 3)];;

urm_run m;;

urm_run_trace m;;*)

(** Tests Rendu 2 **)


let eurm_factorial = [Comment "Compute r1! and place the result in r1"; ZeroPredicate (1, "r1=0");
Goto "r1>0"; Comment "r1 holds 0"; Label "r1=0"; Inc 1; Goto "done"; Comment "r1 holds a positive integer";
Label "r1>0"; Copy (2, 1); Zero 1; Inc 1; Zero 3; Inc 3; Comment "main loop"; Label "loop"; Mult (1, 3);
EqPredicate (2, 3, "done"); Inc 3; Goto "loop"; Label "done"; Quit]


(**** 1er TESTS - ne pas toucher let m = urm_mk prog [Reg (1,5)];; ****)

(*Inc*)
(*let eurm_factorial = [Inc(1); Quit] OK*)
(* let eurm_factorial = [Inc(3); Quit] OK*)

(*Zero*)
(*let eurm_factorial = [Zero(1); Quit] OK*)
(*let eurm_factorial = [Zero(3); Quit] OK*)

(*Copy*)
(* let eurm_factorial = [Copy(1,2); Quit] OK*)
(* let eurm_factorial = [Zero(2);Copy(1,2); Quit] OK *)

(*Quit*)
(*let eurm_factorial = [Quit] OK *)

(*EqPredicate*)
(*let eurm_factorial = [Zero(2); EqPredicate (0, 1, "Done") ; Inc(2) ; Label "Done" ; Quit] OK*)
(*let eurm_factorial = [Zero(1) ; Zero(2); EqPredicate (0, 1, "Done") ; Inc(2) ; Label "Done" ; Quit] OK*)

(*Goto*)
(*let eurm_factorial = [Goto "Done"; Label "Done"; Inc(1) ; Label "Done2" ; Quit] OK *)
(*let eurm_factorial = [Goto "Done2"; Label "Done"; Inc(1) ; Label "Done2" ; Quit] OK *)


(**** 2emes TESTS ****)

(*Add*)
(*let eurm_factorial = [Inc 1; Inc 1; Add(1,3); Quit] A retester*)

let eurm_factorial = [Zero 3; Inc 3; Inc 3; Inc 1; Inc 1; Add(1,3); Quit]

(*GTPredicate*)
(*let eurm_factorial = [GTPredicate(1,3,"Hello"); Label "Hello"; Inc(1); Label "Test"; Quit] OK*)
(*let eurm_factorial = [GTPredicate(1,3,"Test"); Label "Hello"; Inc(1); Label "Test"; Quit] OK*)
(*let eurm_factorial = [GTPredicate(3,1,"Test"); Label "Hello"; Inc(1); Label "Test"; Quit] OK*)

(*Sub*)
(*let eurm_factorial = [Inc 1; Sub(1,3); Quit] *)



(**** 3emes TESTS ***

zero_predicate_out
geqpredicate_out
leqpredicate_out
ltpredicate_out
mult_out
dec_out
*)




let prog = urm_from_eurm eurm_factorial;;

let m = urm_mk prog [Reg (1,5)];;

urm_run m;;
