(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-37-39"]

module Base = struct
  type 'a t = OK of 'a | Err of exn

  let return a = failwith "NYI"
  let bind m f = failwith "NYI"
end

module M = Monad.Expand (Base)
include M
open Base

let err e = failwith "NYI"
let try_with_finally m ks kf = failwith "NYI"
let run m = failwith "NYI"
