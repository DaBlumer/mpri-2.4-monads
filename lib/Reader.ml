(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-37-39"]

module Make (Env : sig
  type t
end) =
struct
  module Base = struct
    type 'a t = Env.t -> 'a

    let return a = fun _ -> a
    let bind (m : 'a t) (f : 'a -> 'b t) : 'b t = fun env -> f (m env) env
  end

  module M = Monad.Expand (Base)
  include M

  let get () = fun env -> env
  let local (e : Env.t) (m : 'a t) = fun _ -> m e
  let run m = m
end
