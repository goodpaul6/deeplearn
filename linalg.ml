type vector = float array
type matrix = { rows : int; cols : int; data : float array }

let ( $. ) mat (row, col) = mat.data.((row * mat.cols) + col)

let mat_of_row_major_arrays arrs =
  let rows = Array.length arrs in
  let cols = Array.length arrs.(0) in
  let data =
    Array.init (rows * cols) (fun i ->
        let r = i / cols in
        let c = i mod cols in
        arrs.(c).(r))
  in
  { rows; cols; data }

(* Each subarray of `mat` is a column *)
let mul_mat_vec mat vec =
  Array.init (Array.length vec) (fun row ->
      let sum = ref 0.0 in
      for col = 0 to mat.cols - 1 do
        sum := !sum +. ((mat $. (row, col)) *. vec.(col))
      done;
      !sum)

let add_vec_vec a b = Array.init (Array.length a) (fun i -> a.(i) +. b.(i))
