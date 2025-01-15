(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-37-39"]

module Make (Log : sig
  type t

  val empty : t
  val ( <+> ) : t -> t -> t
end) =
struct
  open Log

  module Base = struct
    type 'a t = Log.t * 'a

    let return a = (empty, a)
    let bind (log, a) f =
      let (log', b) = f a in
      (log <+> log', b)
  end

  module M = Monad.Expand (Base)
  include M

  let set l = (l, ())
  let run m = m
end
