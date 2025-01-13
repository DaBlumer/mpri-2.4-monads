(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-37-39"]

module Base = struct
  type 'a t = OK of 'a | Err of exn

  let return a = OK a
  let bind (m : 'a t) (f : 'a -> 'b t) = match m with
    | OK a -> f a
    | Err e -> Err e
end

module M = Monad.Expand (Base)
include M
open Base

let err e = Err e
let try_with_finally m f fe = match m with
  | OK a -> f a
  | Err e -> fe e

let run = function
  | OK a -> a
  | Err e -> raise e
