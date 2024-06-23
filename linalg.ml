type vector = float array

(* Data is stored in a column-major format, meaning (row, col) is adjacent to (row, col + 1) *)
type matrix = { rows : int; cols : int; data : float array }

let ( $. ) mat (row, col) = mat.data.((row * mat.cols) + col)

let mat_init rows cols f =
  let data =
    Array.init (rows * cols) (fun i ->
        let r = i / cols in
        let c = i mod cols in
        f r c)
  in
  { rows; cols; data }

let mat_of_arrays arrs =
  assert (Array.length arrs > 0);
  let rows = Array.length arrs in
  let cols = Array.length arrs.(0) in
  mat_init rows cols (fun row col -> arrs.(col).(row))

let mat_transpose mat =
  mat_init mat.cols mat.rows (fun t_row t_col -> mat $. (t_col, t_row))

let mul_mat_vec mat vec =
  assert (Array.length vec = mat.cols);
  Array.init mat.rows (fun row ->
      let sum = ref 0.0 in
      for col = 0 to mat.cols - 1 do
        sum := !sum +. ((mat $. (row, col)) *. vec.(col))
      done;
      !sum)

let add_vec_vec a b = Array.init (Array.length a) (fun i -> a.(i) +. b.(i))

let dot_vec_vec a b =
  let sum = ref 0.0 in
  for i = 0 to Array.length a - 1 do
    sum := !sum +. (a.(i) *. b.(i))
  done;
  !sum
