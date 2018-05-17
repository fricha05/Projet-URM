



(*

	Projet - Machine a registres non limites
	Etudiants - Florian RICHARD et Oscar FALMER
	Fichier - Projet

*)




#use "types.ml";;
#use "partie1_urm.ml";;
#use "partie2_eurm.ml";;
#use "lecture.ml";;
#load "str.cma";;

(** Tests Rendu 1 **)

(*
let fichier =  open_in "test.txt";;

let add_program = program_of_string (string_of_file fichier);;

let m = urm_mk add_program [Reg (1, 2); Reg (2, 3)];;

urm_run m;;

urm_run_trace m;;*)

(** Tests Rendu 2 **)

(**** 1er TESTS - ne pas toucher let m = urm_mk prog [Reg (1,5)];; *)

(*Inc*)
(*let eurm_factorial = [Inc(1); Quit]*)
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
(*let eurm_factorial = [Goto "Done"; Label "Done"; Inc(1) ; Label "Done2" ; Quit]*)
(*let eurm_factorial = [Goto "Done2"; Label "Done"; Inc(1) ; Label "Done2" ; Quit] OK *)


(**** 2emes TESTS ****)

(*GTPredicate OK*)
(*let eurm_factorial = [GTPredicate(1,3,"Hello"); Label "Hello"; Inc(1); Label "Test"; Quit]*)
(*let eurm_factorial = [Zero 3; GTPredicate(1,3,"Test"); Label "Hello"; Inc(1); Label "Test"; Quit] *)
(*let eurm_factorial = [GTPredicate(3,1,"Test"); Label "Hello"; Inc(1); Label "Test"; Quit] *)
(*let eurm_factorial = [Zero 3; GTPredicate(3,1,"Test"); Inc(1); Label "Test"; Quit] *)

(*Add*)
(*let eurm_factorial = [Zero 3; Inc 1; Inc 1; Inc 3; Inc 3; Add(1,3); Quit]*)
(*let eurm_factorial = [Zero 3; Inc 3; Inc 3; Inc 3; Inc 1; Add(1,3); Quit]*)

(*Sub*)
(*let eurm_factorial = [Zero 3; Inc 3; Sub(1,3); Quit]*)


(**** 3emes TESTS ****)

(*LTPredicate*)
(*let eurm_factorial = [Zero 3; LTPredicate(3,1,"Test"); Inc(1); Label "Test"; Quit] *)

(*LEqPredicate*)
(*let eurm_factorial = [Zero 3; LEqPredicate(3,1,"Test"); Inc(1); Label "Test"; Quit]*)
(*let eurm_factorial = [Zero 3; Inc 3; Inc 3; Inc 3; Inc 3; Inc 3; LEqPredicate(3,1,"Test"); Inc(1); Label "Test"; Quit]*)

(*ZeroPredicate*)
(*let eurm_factorial = [Zero 3; ZeroPredicate(3,"Test"); Inc(1); Label "Test"; Quit]*)
(*let eurm_factorial = [Zero 3; Inc 3; ZeroPredicate(3,"Test"); Inc(1); Label "Test"; Quit]*)

(*GEqPredicate*)
(*let eurm_factorial = [Zero 3; GEqPredicate(3,1,"Test"); Inc(1); Label "Test"; Quit] *)
(*let eurm_factorial = [Zero 3; GEqPredicate(1,3,"Test"); Inc(1); Label "Test"; Quit] *)
(*let eurm_factorial = [Zero 3; Inc 3; Inc 3; Inc 3; Inc 3; Inc 3; GEqPredicate(3,1,"Test"); Inc(1); Label "Test"; Quit]*)
(*let eurm_factorial = [Zero 3; Inc 3; Inc 3; Inc 3; Inc 3; Inc 3; Inc 3; GEqPredicate(3,1,"Test"); Inc(1); Label "Test"; Quit]*)

(*Mult*)
(*let eurm_factorial = [Zero 3; Inc 3; Inc 3; Mult(1,3); Quit] *)
(*let eurm_factorial = [Zero 3; Inc 3; Inc 3; Inc 3; Inc 3; Mult(1,3); Quit]*)

(*Dec*)
(*let eurm_factorial = [Zero 3; Inc 3; Dec 3; Dec 1; Quit]*)


let eurm_factorial = [Comment "Compute r1! and place the result in r1"; ZeroPredicate (1, "r1=0");
Goto "r1>0"; Comment "r1 holds 0"; Label "r1=0"; Inc 1; Goto "done"; Comment "r1 holds a positive integer";
Label "r1>0"; Copy (2, 1); Zero 1; Inc 1; Zero 3; Inc 3; Comment "main loop"; Label "loop"; Mult (1, 3);
EqPredicate (2, 3, "done"); Inc 3; Goto "loop"; Label "done"; Quit]

let prog = urm_from_eurm eurm_factorial;;

let m = urm_mk prog [Reg (1,5)];;

urm_run m;;
