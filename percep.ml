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
    (* If error is positive, what does that mean? Means our guess was -1 when
       it should've been 1. That means weight * input was probably too low.

       What proportion of that error was this weight's responsibility?

       Well, this weight is multiplied by input to get the sum, so whatever the
       input is that's the "proportion" of the error this weight is responsible for.
       Hence, we adjust the weight by error * input, adding in the learning rate
       so as not to overshoot. *)
    t.weights.(i) <- t.weights.(i) +. (error *. inputs.(i) *. t.learning_rate)
  done
