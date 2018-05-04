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
	|URMCopy of regidx * regidx
	|URMJump of regidx * regidx * line
	|URMSucc of regidx
	|URMZero of regidx

(* instruction pointer *)
type instptr = InstPtr of (line * urmcmd) list * (line * urmcmd) list

(* URM *)
type urm = {instptr : instptr; regs : reg list};;

let add_program = [URMZero 0; URMZero 3; URMJump (1, 3, 6); URMSucc 0; URMSucc 3; URMJump (3, 3, 2); URMZero 3; URMJump (2, 3, 11); URMSucc 0; URMSucc 3; URMJump (3, 3, 7)];;


(*EURM*)

(* label *)
type label = string

(* EURM instruction *)
type eurmcmd =
	|Add of regidx * regidx
	|Comment of string
	|Copy of regidx * regidx
	|Dec of regidx
	|EqPredicate of regidx * regidx * label
	|GEqPredicate of regidx * regidx * label
	|GTPredicate of regidx * regidx * label
	|Goto of label
	|Inc of regidx
	|Label of label
	|LEqPredicate of regidx * regidx * label
	|LTPredicate of regidx * regidx * label
	|Mult of regidx * regidx
	|Quit
	|Sub of regidx * regidx
	|Zero of regidx
	|ZeroPredicate of regidx * label




