open Monads
open Monoid

module Make (Log : Monoid) = struct
  (* Use the Update monad to instantiate the following definitions *)

  module UnitS = struct
    type t = unit
    type m = Log.t
    let act _ _ = ()
  end
  module Writer = Update.Make(Log)(UnitS)

  type 'a t = 'a Writer.t

  let return = Writer.return
  let bind = Writer.bind
  let ( >>= ) = Writer.( >>= )
  let ( let* ) = Writer.( let* )
  let set = Writer.set
  let run x = Writer.run x ()
end
