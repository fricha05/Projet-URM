(* line number *)
type line = int

(* register index *)
type regidx = int

(* register value *)
type regval = int

(* register *)
type reg = Reg of regidx * regval

(* URM instruction *)
type urmcmd =
	|Copy of regidx * regidx
	|Jump of regidx * regidx * line
	|Succ of regidx
	|Zero of regidx

(* instruction pointer *)
type instptr = InstPtr of (line * urmcmd) list * (line * urmcmd) list

(* URM *)
type urm = {instptr : instptr; regs : reg list};;

let add_program = [Zero 0; Zero 3; Jump (1, 3, 6); Succ 0; Succ 3; Jump (3, 3, 2); Zero 3; Jump (2, 3, 11); Succ 0; Succ 3; Jump (3, 3, 7)];;
