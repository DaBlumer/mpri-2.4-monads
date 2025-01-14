(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-20-27-32-33-37-39"]

open Monads.Nondeterminism

let rec insert x l = match l with
  | [] -> return [x]
  | hd::tl ->
     let* b = insert x tl in
     either (return (x::l)) (return (hd::b))

let rec permut l = match l with
  | [] -> return []
  | hd::tl ->
     let* tl = permut tl in
     insert hd tl

let%test _ = List.of_seq (all (permut [])) = [ [] ]
let%test _ = List.of_seq (all (permut [ 1 ])) = [ [ 1 ] ]

let%test _ =
  List.sort compare (List.of_seq (all (permut [ 1; 2 ])))
  = [ [ 1; 2 ]; [ 2; 1 ] ]

let%test _ =
  List.sort compare (List.of_seq (all (permut [ 1; 2; 3 ])))
  = [
      [ 1; 2; 3 ];
      [ 1; 3; 2 ];
      [ 2; 1; 3 ];
      [ 2; 3; 1 ];
      [ 3; 1; 2 ];
      [ 3; 2; 1 ];
    ]
