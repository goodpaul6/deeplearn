(* Generates data for a toy classifier which
   classifies points above y = x as +1 and below as -1 *)

type labeled_point = { x : float; y : float; label : [ `Above | `Below ] }

let init count x_fn y_fn =
  Array.init count (fun i ->
      let x = x_fn i in
      let y = y_fn i in
      let line_y = x in
      { x; y; label = (if y >= line_y then `Above else `Below) })

let init_rand count =
  let f _ = Random.float 1.0 in
  init count f f
