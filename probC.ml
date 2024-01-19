type color = W | B

type image =
  | L of color
  | N of image * image * image * image

exception InvalidInput

let is_power_of_two n =
  n <> 0 && (n land (n - 1)) = 0

let validate_dimensions n p =
  if not (is_power_of_two n && p = n && 0 < p && p <= 1024)
  then raise InvalidInput

let rec build_quadtree image =
  let n = Array.length image in
  let rec build_quadtree_aux x y size =
    let base_color = image.(y).(x) in
    let uniform_color = ref true in
    for i = 0 to size - 1 do
      for j = 0 to size - 1 do
        if image.(y + i).(x + j) <> base_color then
          uniform_color := false
      done;
    done;
    if !uniform_color then
      if base_color = 0 then
        L W
      else
        L B
    else
      let half_size = size / 2 in
      let nw = build_quadtree_aux x y half_size in
      let ne = build_quadtree_aux (x + half_size) y half_size in
      let sw = build_quadtree_aux x (y + half_size) half_size in
      let se = build_quadtree_aux (x + half_size) (y + half_size) half_size in
      N (nw, ne, sw, se)
  in
  build_quadtree_aux 0 0 n

(* O restante do código permanece o mesmo *)


let rec quadtree_to_matrix tree size =
  let matrix = Array.make_matrix size size 0 in
  match tree with
  | L W -> matrix
  | L B -> Array.map (Array.map (fun _ -> 1)) matrix
  | N (nw, ne, sw, se) ->
    let half_size = size / 2 in
    let fill_submatrix quadtree x y =
      let submatrix = quadtree_to_matrix quadtree half_size in
      for i = 0 to half_size - 1 do
        for j = 0 to half_size - 1 do
          matrix.(y + i).(x + j) <- submatrix.(i).(j);
        done;
      done
    in
    fill_submatrix nw 0 0;
    fill_submatrix ne half_size 0;
    fill_submatrix sw 0 half_size;
    fill_submatrix se half_size half_size;
    matrix

let print_image image =
  let size = Array.length image in
  Printf.printf "P1\n";
  Printf.printf "%d %d\n" size size;
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      Printf.printf "%d " image.(i).(j)
    done;
    if i < size - 1 then print_newline ()
  done

let rec count_leaves_and_nodes tree =
  match tree with
  | L _ -> (1, 0)
  | N (nw, ne, sw, se) ->
    let leaves_nw, nodes_nw = count_leaves_and_nodes nw in
    let leaves_ne, nodes_ne = count_leaves_and_nodes ne in
    let leaves_sw, nodes_sw = count_leaves_and_nodes sw in
    let leaves_se, nodes_se = count_leaves_and_nodes se in
    (leaves_nw + leaves_ne + leaves_sw + leaves_se,
     1 + nodes_nw + nodes_ne + nodes_sw + nodes_se)

let rec calculate_branch_sizes tree =
  let rec calculate_branch_sizes_aux tree depth min_size max_size =
    match tree with
    | L _ -> (min_size, max_size)
    | N (nw, ne, sw, se) ->
      let min_size' = min min_size (depth + 1) in
      let max_size' = max max_size (depth + 1) in
      let min_size_nw, max_size_nw = calculate_branch_sizes_aux nw (depth + 1) min_size' max_size' in
      let min_size_ne, max_size_ne = calculate_branch_sizes_aux ne (depth + 1) min_size_nw max_size_nw in
      let min_size_sw, max_size_sw = calculate_branch_sizes_aux sw (depth + 1) min_size_ne max_size_ne in
      calculate_branch_sizes_aux se (depth + 1) min_size_sw max_size_sw
  in
  calculate_branch_sizes_aux tree 0 max_int 0

let rotate_left tree =
  let rec rotate_left_aux tree =
    match tree with
    | L _ -> tree
    | N (nw, ne, sw, se) ->
      let nw' = rotate_left_aux nw in
      let ne' = rotate_left_aux ne in
      let sw' = rotate_left_aux sw in
      let se' = rotate_left_aux se in
      N (ne', se', nw', sw')
  in
  rotate_left_aux tree

let invert_colors tree =
  let rec invert_colors_aux tree =
    match tree with
    | L W -> L B
    | L B -> L W
    | N (nw, ne, sw, se) ->
      let nw' = invert_colors_aux nw in
      let ne' = invert_colors_aux ne in
      let sw' = invert_colors_aux sw in
      let se' = invert_colors_aux se in
      N (nw', ne', sw', se')
  in
  invert_colors_aux tree

let rotate_right tree =
  let rec rotate_right_aux tree =
    match tree with
    | L _ -> tree
    | N (nw, ne, sw, se) ->
      let nw' = rotate_right_aux nw in
      let ne' = rotate_right_aux ne in
      let sw' = rotate_right_aux sw in
      let se' = rotate_right_aux se in
      N (se', sw', ne', nw')
  in
  rotate_right_aux tree

let read_input () =
  let filetype = read_line () in
  if filetype <> "P1" then raise InvalidInput;
  let dimensions = read_line () |> String.split_on_char ' ' |> List.map int_of_string in
  let n, p = match dimensions with
    | x :: y :: [] -> x, y
    | _ -> raise InvalidInput
  in
  validate_dimensions n p;
  let matrix = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    let row = read_line () |> String.split_on_char ' ' |> List.map int_of_string in
    for j = 0 to n - 1 do
      matrix.(i).(j) <- List.nth row j;
    done;
  done;
  matrix

let rec print_quadtree_and_convert_back quadtree size =
  let matrix' = quadtree_to_matrix quadtree size in
  print_image matrix'

let () =
try
  let matrix = read_input () in
  let quadtree = build_quadtree matrix in
  let leaves, nodes = count_leaves_and_nodes quadtree in
  let min_size, max_size = calculate_branch_sizes quadtree in
  Printf.printf "%d %d\n" leaves nodes;
  Printf.printf "%d %d\n" min_size max_size;
  let rotated_left = rotate_left quadtree in
  let inverted = invert_colors rotated_left in
  let rotated_right = rotate_right inverted in
  let size = Array.length matrix in
  print_quadtree_and_convert_back rotated_left size;
  print_newline ();
  print_quadtree_and_convert_back inverted size;
  print_newline ();
  print_quadtree_and_convert_back rotated_right size;
  print_newline ()
with
| InvalidInput -> Printf.printf "Invalid input.\n"








(*


0 0 0 0 0 1 0 1
0 0 0 0 0 0 0 1
0 0 0 0 0 0 1 1
0 0 0 0 0 1 1 1
0 0 0 0 1 1 1 1
0 0 0 0 1 1 1 1
0 0 0 1 1 1 1 1
0 0 1 1 1 1 1 1









22 7
1 3
P1
8 8
0 1 1 1 1 1 1 1
1 0 1 1 1 1 1 1
0 0 0 1 1 1 1 1
1 0 0 0 1 1 1 1
0 0 0 0 0 0 1 1
0 0 0 0 0 0 0 1
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
P1
8 8
1 0 0 0 0 0 0 0
0 1 0 0 0 0 0 0
1 1 1 0 0 0 0 0
0 1 1 1 0 0 0 0
1 1 1 1 1 1 0 0
1 1 1 1 1 1 1 0
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
P1
8 8
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
0 1 1 1 1 1 1 1
0 0 1 1 1 1 1 1
0 0 0 0 1 1 1 0
0 0 0 0 0 1 1 1
0 0 0 0 0 0 1 0
0 0 0 0 0 0 0 1
*)