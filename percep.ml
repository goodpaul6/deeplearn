type t = { weights : float array }

let init n fn = { weights = Array.init n fn }
let init_rand n = init n (fun _ -> Random.float 2.0 -. 1.0)

let guess t inputs =
  let sum = Linalg.dot_vec_vec t.weights inputs in
  if sum >= 0.0 then 1.0 else -1.0
