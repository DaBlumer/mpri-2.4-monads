(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-32-33"]

open Monads

module Make (Env : sig
  type t

  val init : t
end) =
struct
  (* Use the Update monad to instantiate the following definitions *)
  module M = struct
    type t = Env.t list
    let empty = []
    let (<+>) = List.append
  end

  module UnitS = struct
    type t = unit
    type m = M.t
    let act _ _ = ()
  end

  module Reader = Update.Make (M) (UnitS)
  include Reader
  (* NYI: bring me in scope! *)
end
