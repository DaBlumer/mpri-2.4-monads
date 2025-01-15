(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-20-27-32-33-37-39"]

open Monads

(* Taken from https://chrispenner.ca/posts/update-monad *)

module Transaction = struct
  type t =
    | Deposit of int * t
    | Withdraw of int * t
    | ApplyInterest of t
    | EndOfTransaction

  let empty = EndOfTransaction

  (* TODO: not tail rec *)
  let rec ( <+> ) t1 t2 =
    match t1 with
    | Deposit (i, t1) -> Deposit (i, t1 <+> t2)
    | Withdraw (i, t1) -> Withdraw (i, t1 <+> t2)
    | ApplyInterest t1 -> ApplyInterest (t1 <+> t2)
    | EndOfTransaction -> t2
end

module State = struct
  open Transaction

  type m = Transaction.t
  type t = int

  (* TODO: should use an integral representation instead: manipulate 'balance * 100' *)
  let compute_interest balance = int_of_float (float_of_int balance *. 1.1)
  let rec act balance = function
    | Deposit (i, t) -> act (balance + i) t
    | Withdraw (i, t) -> act (balance - i) t
    | ApplyInterest t -> act (compute_interest balance) t
    | EndOfTransaction -> balance
end

open Transaction

module Upd = Update.Make(Transaction)(State)

open Upd
(*let ( let* ) _ _ = failwith "NYI: bring me in scope!"
let get _ = failwith "NYI: bring me in scope!"
let run _ = failwith "NYI: bring me in scope!"*)

let deposit s = set (Deposit (s, EndOfTransaction))
let withdraw s = set (Withdraw (s, EndOfTransaction))
let apply_interest () = set (ApplyInterest EndOfTransaction)

let use_ATM () =
  let* _ = deposit 20 in
  let* _ = deposit 30 in
  let* _ = apply_interest () in
  let* _ = withdraw 10 in
  get ()

let%test _ =
  let receipt, balance = run (use_ATM ()) 0 in
  balance = 45
  && receipt
     = Deposit
         (20, Deposit (30, ApplyInterest (Withdraw (10, EndOfTransaction))))
