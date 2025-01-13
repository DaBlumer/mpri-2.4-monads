(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-20-27-32-37-39"]
[@@@warning "-38"]

open Monads

type value = IsNat of int | IsBool of bool

type exp =
  | Val of value
  | Eq of exp * exp
  | Plus of exp * exp
  | Ifte of exp * exp * exp

exception IllTyped


let ( let* ) = Error.(let*)
let return = Error.return
let run = Error.run
let err = Error.err IllTyped
let rec sem e : value Error.t = match e with
  | Val x -> return x
  | Eq (x, y) ->
      let* x = sem x in
      let* y = sem y in
      begin match x, y with
      | IsNat i1, IsNat i2 -> IsBool (i1 = i2) |> return
      | IsBool b1, IsBool b2 -> IsBool (b1 = b2) |> return
      | _ -> IsBool false |> return
      end
  | Plus (n, m) ->
      let* n = sem n in
      let* m = sem m in
      begin match n, m with
      | IsNat n, IsNat m -> IsNat (n+m) |> return
      | _ -> err
      end
  | Ifte (b, x, y) ->
      let* b = sem b in
      let* x = sem x in
      let* y = sem y in
      begin match b with
       |IsBool b ->
        begin match (x, y) with
          | IsBool x, IsBool y -> IsBool (if b then x else y) |> return
          | IsNat x, IsNat y -> IsNat (if b then x else y) |> return
          | _ -> err
        end
       |_ -> err
      end

(** * Tests *)

let%test _ = run (sem (Val (IsNat 42))) = IsNat 42
let%test _ = run (sem (Eq (Val (IsBool true), Val (IsBool true)))) = IsBool true
let%test _ = run (sem (Eq (Val (IsNat 3), Val (IsNat 3)))) = IsBool true

let%test _ =
  run (sem (Eq (Val (IsBool true), Val (IsBool false)))) = IsBool false

let%test _ = run (sem (Eq (Val (IsNat 42), Val (IsNat 3)))) = IsBool false

let%test _ =
  (* Alternatively: one could have an exception *)
  run (sem (Eq (Val (IsNat 42), Val (IsBool false)))) = IsBool false

let%test _ = run (sem (Plus (Val (IsNat 42), Val (IsNat 3)))) = IsNat 45

let%test _ =
  run (sem (Ifte (Val (IsBool true), Val (IsNat 42), Val (IsNat 3)))) = IsNat 42

let%test _ =
  run (sem (Ifte (Val (IsBool false), Val (IsNat 42), Val (IsNat 3)))) = IsNat 3

let%test _ =
  run
    (sem
       (Ifte
          ( Eq (Val (IsNat 21), Plus (Val (IsNat 20), Val (IsNat 1))),
            Val (IsNat 42),
            Val (IsNat 3) )))
  = IsNat 42

(** ** Ill-typed expressions *)

let%test _ =
  try
    ignore (run (sem (Plus (Val (IsBool true), Val (IsNat 3)))));
    false
  with
  | IllTyped -> true
  | _ -> false

let%test _ =
  try
    ignore (run (sem (Ifte (Val (IsNat 3), Val (IsNat 42), Val (IsNat 44)))));
    false
  with
  | IllTyped -> true
  | _ -> false
