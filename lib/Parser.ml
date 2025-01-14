(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-37-39"]

module Base = struct
  type 'a res = Val of 'a * char list | Err
  type 'a t = char list -> 'a res

  let return a = (fun buf -> Val (a, buf))
  let bind m f = fun buf ->
   match m buf with
   | Err -> Err
   | Val (a, buf') -> (f a) buf'
end

module M = Monad.Expand (Base)
include M
open Base


let fail () = (fun buf -> Err)
let any () = function | hd::tl -> Val(hd, tl) | [] -> Err
let empty () = function [] -> Val ((), []) | _ -> Err

let symbol c = failwith "NYI"

let either m1 m2 = fun buf ->
  match m1 buf, m2 buf with
  | (Val (_,_) as v), _ | _, (Val (_,_) as v) -> v
  | Err, Err -> Err

let optionally m = failwith "NYI"

let rec star m = failwith "NYI"
and plus m = failwith "NYI"

let run m toks = match m toks with
  | Err -> raise (Failure "parsing error")
  | Val(v, _) -> v

(* TODO: add a backtracking operator? *)
