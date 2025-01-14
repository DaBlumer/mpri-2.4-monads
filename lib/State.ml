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
    let bind m f = fun (s : S.t) ->
      let (a : 'a), (s : S.t) = m s in
      let mb : 'b t = f a in
      mb s
  end

  module M = Monad.Expand (State)
  include M

  let get () s = (s, s)
  let set x _ = ((), x)
  let run m s = m s |> fst
end
