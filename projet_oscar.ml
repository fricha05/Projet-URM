
// 1

let rec compile_label_out is =
	let rec aux is acc =
		match is with (* is: instructions *)
		|[] -> []
		|Label s::is' -> ("Label " ^ acc) :: (aux is' (acc+1))
		|i::is' -> i::(aux is' acc)
	in aux is 1;
;;

// 2

let rec dec_out is =
    match is with
    |[] -> []
    |Dec(ri)::is' -> Sub(ri,1)::dec_out is' (*ri : registre index*)
    |i::is' -> i::dec_out is'
;;

let rec mult_out is =
    match is with
    |[] -> []
    |Mult(ri1,ri2)::is' -> let rec aux is =
    						in Add(s,1)::(aux )::mult_out is'
    |i::is' -> i::mult_out is'
;;
