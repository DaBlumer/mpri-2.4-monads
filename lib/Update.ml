open Monoid

(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-37-39"]

module Make (P : Monoid) (S : MonoidAction with type m = P.t) = struct
  module Base = struct
    open P
    open S

    type 'a t = S.t -> P.t * 'a

    let return a = fun _ -> (empty, a)
    let bind (m : 'a t) (f : 'a -> 'b t) : 'b t = fun (s : S.t) ->
      let (x, a) = m s in
      let (x', b) = (f a) (act s x) in
      (x <+> x', b)
  end

  module M = Monad.Expand (Base)
  include M

  let get () : S.t t = fun s -> (P.empty, s)
  let set p = fun s -> (p, ())
  let run m = m
end
