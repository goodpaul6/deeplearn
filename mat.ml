type vector = float array

(* Column-major storage, so (row, col) is next to (row, col + 1) *)
type matrix = { rows : int; cols : int; data : float array }

let ( $. ) mat (row, col) = mat.data.((row * mat.cols) + col)

let mat_of_arrays arr =
  let rows = Array.length arr in
  let cols = Array.length arr.(0) in
  let data =
    Array.init (rows * cols) (fun i ->
        let r = i / cols in
        let c = i mod cols in
        arr.(r).(c))
  in
  { rows; cols; data }

let mul_mat_vec mat vec =
  let vec_len = Array.length vec in
  assert (vec_len = mat.cols);
  Array.init vec_len (fun mat_row ->
      let sum = ref 0.0 in
      for mat_col_vec_row = 0 to mat.cols - 1 do
        let mat_value = mat $. (mat_row, mat_col_vec_row) in
        sum := !sum +. (mat_value *. vec.(mat_col_vec_row))
      done;
      !sum)

let add_vec_vec a b = Array.init (Array.length a) (fun i -> a.(i) +. b.(i))
let sub_vec_vec a b = Array.init (Array.length a) (fun i -> a.(i) -. b.(i))

let dot_vec_vec a b =
  let sum = ref 0.0 in
  for i = 0 to Array.length a - 1 do
    sum := a.(i) *. b.(i)
  done;
  !sum

let len2_vec a = dot_vec_vec a a
