(* Generates data for a toy classifier which
   classifies points above y = x as +1 and below as -1 *)

type labeled_point = { x : float; y : float; label : float }

let init count x_fn y_fn above_fn =
  List.init count (fun i ->
      let x = x_fn i in
      let y = y_fn i in
      { x; y; label = (if above_fn x y then 1.0 else -1.0) })

let init_rand count above_fn =
  let f _ = Random.float 2.0 -. 1.0 in
  init count f f above_fn
