(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-37-39"]

module Make (S : sig
  type t
end) =
struct
  module State = struct
    type 'a t = S.t -> 'a * S.t

    let return a = (fun x -> a, x)
    let bind m f = fun s ->
      let (a : 'a), (s = m s in
      let sb = f a in
      fun 
  end (* Pierre-Marie Pédrot / *)

  module M = Monad.Expand (State)
  include M

  let get () s = failwith "NYI"
  let set x _ = failwith "NYI"
  let run m = failwith "NYI"
end
