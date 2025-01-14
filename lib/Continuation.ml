(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-37-39"]

module Make (Ans : sig
  type t
end) =
struct
  module Base = struct
    type 'a t = ('a -> Ans.t) -> Ans.t

    let return a = fun f -> f a
    let bind (m : ('a -> Ans.t) -> Ans.t) (f : 'a -> 'b t) =
     fun (fb : 'b -> Ans.t) ->
       m (fun (a:'a) -> (f a) fb)
  end

  module M = Monad.Expand (Base)
  include M

  let callcc f (c : ('a -> Ans.t)) : Ans.t = (f c) c
  let throw m k' = (fun k -> m k')
  let tfix mrec a = failwith "NYI"
  let run m = m (fun x -> x)
end
