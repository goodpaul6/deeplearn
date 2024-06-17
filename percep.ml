type t = { weights : float array; learning_rate : float }

let init n fn = { weights = Array.init n fn; learning_rate = 0.1 }
let init_rand n = init n (fun _ -> Random.float 2.0 -. 1.0)

let guess t inputs =
  let sum = Linalg.dot_vec_vec t.weights inputs in
  if sum >= 0.0 then 1.0 else -1.0

let train t inputs target =
  let guess = guess t inputs in
  let error = target -. guess in
  for i = 0 to Array.length inputs - 1 do
    t.weights.(i) <- error *. t.weights.(i) *. t.learning_rate
  done
