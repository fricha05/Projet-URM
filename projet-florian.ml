let rec compile_comment_out is =
    match is with
    |[] -> []
    |Comment s::is' -> compile_comment_out is'
    |i::is' -> i::compile_comment_out is'
;;

let compile_comment_out' is =
  let is_not_comment i =
    match i with
    |Comment _ -> false
    |_ -> true
    in List.filter is_not_comment is
;;
