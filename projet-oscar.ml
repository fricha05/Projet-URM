
// 1

let rec compile_label_out is =
	let rec aux is acc =
		match is with (* is: instructions *)
		|[] -> []
		|Label s::is' -> ("Label " ^ acc) :: (aux is' (acc+1))
		|i::is' -> i::(aux is' acc)
	in aux is 1;
;;

// Etape 1

let rec dec_out is =
    match is with
    |[] -> []
    |Dec(ri)::is' -> Sub(ri,1)::dec_out is' (*ri : registre index*)
    |i::is' -> i::dec_out is'
;;

let rec mult_out is =
    match is with
    |[] -> []
    |Mult(ri1,ri2)::is' -> if r1 <> 1 then Add(ri1,ri1)::mult_out (Mult(ri1, (ri2-1))::is')
    						else mult_out is'
    |i::is' -> i::mult_out is'
;;

let rec zero_predicate_out is = 
	match is with
	|[] -> []
	|ZeroPredicate(ri,label)::is' -> EqPredicate(ri, 0, label)::zero_predicate_out is'
	|i::is' -> i::zero_predicate_out is'
;;

let rec geqpredicate_out is = 
	match is with
	|[] -> []
	|GEqPredicate(ri1,ri2,label)::is' -> EqPredicate(ri1, ri2, label)::GTPredicate(ri1, ri2, label)::geqpredicate_out is'
	|i::is' -> i::geqpredicate_out is'
;;

let rec leqpredicate_out is = 
	match is with
	|[] -> []
	|LEqPredicate(ri1,ri2,label)::is' -> EqPredicate(ri1, ri2, label)::GTPredicate(ri2, ri1, label)::leqpredicate_out is'
	|i::is' -> i::leqpredicate_out is'
;;

let rec ltpredicate_out is = 
	match is with
	|[] -> []
	|LTqPredicate(ri1,ri2,label)::is' -> GTPredicate(ri2, ri1, label)::ltpredicate_out is'
	|i::is' -> i::ltpredicate_out is'
;;

let compile_stage1 is = ltpredicate_out (leqpredicate_out (geqpredicate_out (zero_predicate_out (mult_out (dec_out is)))));;

// Etape 2

let rec add_out is = 
	match is with
	|[] -> []
	|Add(ri1,ri2)::is' -> if r2 <> 0 then Inc(ri1)::add_out (Add(ri1, (ri2-1))::is')
    						else add_out is'
	|i::is' -> i::ltpredicate_out is'
;;

let rec sub_out is = 
	match is with
	|[] -> []
	|Sub(ri1,ri2)::is' -> Add(0,(ri1-ri2)::sub_out is'
	|i::is' -> i::sub_out is'
;;

let rec gtpredicate_out is = 
	match is with
	|[] -> []
	|GTqPredicate(ri1, ri2, label)::is' -> Add(0,(ri1-ri2)::gtpredicate_out is'
	|i::is' -> i::gtpredicate_out is'
;;

let compile_stage2 is = gtpredicate_out( sub_out( add_out(is) ) );;

// Etape 3

// Etape 4


















