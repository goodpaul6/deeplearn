(* Each subarray of `mat` is a column *)
let dot mat vec =
  let cols = Array.length mat in
  let rows = Array.length mat.(0) in
  let vec_len = Array.length vec in
  let new_vec = Array.make vec_len 0.0 in
  assert (vec_len = cols);
  for i = 0 to rows - 1 do
    let sum = ref 0.0 in
    for j = 0 to cols - 1 do
      sum := !sum +. (mat.(j).(i) *. vec.(j))
    done;
    new_vec.(i) <- !sum
  done;
  new_vec

let add a b =
  let a_len = Array.length a in
  let b_len = Array.length b in
  assert (a_len = b_len);
  let new_vec = Array.make a_len 0.0 in
  for i = 0 to a_len - 1 do
    new_vec.(i) <- a.(i) +. b.(i)
  done;
  new_vec
