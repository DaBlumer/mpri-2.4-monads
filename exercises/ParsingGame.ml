(* Once you are done writing the code, remove this directive,
      whose purpose is to disable several warnings. *)
[@@@warning "-20-27-32-33-37-39"]

open Monads

(* Taken from [https://wiki.haskell.org/State_Monad#Complete_and_Concrete_Example_1] *)

(* Passes a string of dictionary {a,b,c}
 * Game is to produce a number from the string.
 * By default the game is off, a 'c' toggles the
 * game on and off.
 * A 'a' gives +1 and a 'b' gives -1 when the game is on,
 * nothing otherwise.
 *)

module GameState = State.Make(struct type t = (bool * int) end)
open GameState
let rec play_game s (active, count) =
  let open String in
  let split_s s = get s 0, if length s = 1 then "" else sub s 1 (length s - 1)
  if length s = 0 then "", (active, count) else
  let (c, s) = split s in play_game s
  begin match get s 0 with
  | 'c' -> (not active, count)
  | 'a' -> (active, count+1)
  | 'b' -> (active, count-1)
  | _ -> assert false
  end
let result s = run (play_game s) (false, 0)

let result2 s1 s2 =
  let p =
    let* _ = play_game s1 in
    (* State is kept between [s1] and [s2]! *)
    let* score = play_game s2 in
    return score
  in
  run p (false, 0)

let%test _ = result "ab" = 0
let%test _ = result "ca" = 1
let%test _ = result "cabca" = 0
let%test _ = result "abcaaacbbcabbab" = 2
let%test _ = result2 "ab" "ca" = 1
let%test _ = result2 "ca" "abcaaacbbcabbab" = -1
