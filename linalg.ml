type vector = float array

(* Data is stored in a column-major format, meaning (row, col) is adjacent to (row, col + 1) *)
type matrix = { rows : int; cols : int; data : float array }

let ( $. ) mat (row, col) = mat.data.((row * mat.cols) + col)

let mat_of_arrays arrs =
  let rows = Array.length arrs in
  let cols = Array.length arrs.(0) in
  let data =
    Array.init (rows * cols) (fun i ->
        let r = i / cols in
        let c = i mod cols in
        arrs.(c).(r))
  in
  { rows; cols; data }

let mul_mat_vec mat vec =
  Array.init (Array.length vec) (fun row ->
      let sum = ref 0.0 in
      for col = 0 to mat.cols - 1 do
        sum := !sum +. ((mat $. (row, col)) *. vec.(col))
      done;
      !sum)

let add_vec_vec a b = Array.init (Array.length a) (fun i -> a.(i) +. b.(i))

let dot_vec_vec a b =
  let sum = ref 0.0 in
  for i = 0 to Array.length a - 1 do
    sum := a.(i) *. b.(i)
  done;
  !sum
