open Lib

let pad_lock = [|
  [|None; Some(2); Some(3)|];
  [|Some(4); None; Some(6)|];
  [|Some(7); Some(8); Some(9)|];
|]

let moves = [
  (2, 1); (-2, 1);
  (2, -1); (-2, -1);
  (1, 2); (1, -2);
  (-1, 2); (-1, -2);
]

let pos_solutions (i, j) =
  List.fold_right (fun (im, jm) ls ->
      let inw = i + im in
      let jnw = j + jm in
      if inw >= 0 && inw < 3 && jnw >= 0 && jnw < 3 then
        match pad_lock.(inw).(jnw) with
        | Some _ ->
          (inw, jnw)::ls
        | None -> ls
      else
        ls
    ) moves []

let rec find_n_combinations ((i, j) as idx) depth max_depth =
  if depth > max_depth then
    match pad_lock.(i).(j) with
    | Some _ -> Tree.leaf (depth, idx)
    | None -> Empty
  else
    let pos = pos_solutions idx in
    let childs =
      List.fold_right (fun elem nodes ->
          let child = find_n_combinations elem (depth + 1) max_depth in
          match child with
          | Node _ as inner ->
            inner::nodes
          | _ -> nodes
        ) pos [] in
    Node ((depth, idx), childs)


let () =
  (find_n_combinations (0, 2) 0 4)
  |> Tree.paths
  |> List.filter (fun ls -> (List.length ls) = 4)
  |> List.iter (fun path ->
      print_string "Path >>\n";
      path
      |> List.map (fun (_, (i, j)) -> pad_lock.(i).(j))
      |> List.iter (function
          | Some n -> Printf.printf "%d\n" n
          | None -> print_string "" )
    )
