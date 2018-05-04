
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
    |Mult(ri1,ri2)::is' -> Add(ri1,ri1)::mult_out (Mult(ri1, (ri2-1))::is')
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



















