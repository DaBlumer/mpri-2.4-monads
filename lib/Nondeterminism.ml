(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-37-39"]

module Base = struct
  type 'a t = 'a list

  let return a = [a]
  let bind m f = List.concat_map f m
end

module M = Monad.Expand (Base)
include M

let fail () = []
let either a b = a @ b
let run = List.hd
let all = List.to_seq
