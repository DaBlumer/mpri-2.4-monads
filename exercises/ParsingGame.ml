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
let rec play_game s =
  let split s = let open String in
    get s 0, if length s = 1 then "" else sub s 1 (length s - 1)
  in
  let* (active, count) = get () in
  (*let _ = Format.printf "(%b,%d), %s\n" active count s in*)
  if String.length s = 0 then return count else
  let (c, s) = split s in
  let* _ = set
    begin match c with
    | 'c' -> (not active, count)
    | 'a' -> (active, if active then count+1 else count)
    | 'b' -> (active, if active then count-1 else count)
    | _ -> assert false
    end
  in
  let* _ = play_game s in
  let* (_, count) = get () in
  return count
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
