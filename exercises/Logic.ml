(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-37-39"]

open Monads

type bot = |
type 'a not = 'a -> bot

module M = Continuation.Make (struct
  type t = bot
end)

open M



(*
  I have a hard time understanting/intuiting continuations/callcc
  The thing I did get is that the Continuation monad's underlying
  functor (instantiated with bot) sends a type to its double
  negation translation. I only used this intuition to prove
  the double negation of the excluded middle.
*)
let exfalso () =
  fun (f : ('a, 'a not) result -> bot) ->
    f (Error (fun (a : 'a) -> f (Ok a)))
