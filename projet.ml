



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

(*let eurm_factorial = [Dec 0; Quit]*)

let prog = urm_from_eurm eurm_factorial;;

let m = urm_mk prog [];;

urm_run m;;
