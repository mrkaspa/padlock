open Lib
open Core

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

module Tuple_map = Map.Make (struct
    type t = int * int
    include Tuple.Comparable (Int) (Int)
  end)

let pos_solutions ((i, j) as idx) (cache : (int * int) list Tuple_map.t) =
  match Tuple_map.find cache idx with
  | Some sols -> (sols, cache)
  | None ->
    let sols =
      List.fold_right ~f:(fun (im, jm) ls ->
          let inw = i + im in
          let jnw = j + jm in
          if inw >= 0 && inw < 3 && jnw >= 0 && jnw < 3 then
            match pad_lock.(inw).(jnw) with
            | Some _ ->
              (inw, jnw)::ls
            | None -> ls
          else
            ls
        ) ~init:[] moves
    in
    (sols, Tuple_map.add_exn cache ~key:idx ~data:sols)

let rec find_n_combinations ((i, j) as idx) depth max_depth (cache : (int * int) list Tuple_map.t) =
  if depth > max_depth then
    match pad_lock.(i).(j) with
    | Some _ -> Tree.leaf (depth, idx)
    | None -> Empty
  else
    let (pos, cache) = pos_solutions idx cache in
    let children =
      List.fold_right ~f:(fun elem nodes ->
          let child = find_n_combinations elem (depth + 1) max_depth cache in
          match child with
          | Node _ as inner ->
            inner::nodes
          | _ -> nodes
        ) ~init:[] pos
    in
    Tree.node (depth, idx) children

let () =
  (find_n_combinations (0, 2) 0 4 (Tuple_map.empty))
  |> Tree.paths
  |> List.filter ~f:(fun ls -> (List.length ls) = 4)
  |> List.iter ~f:(fun path ->
      print_string "Path >>\n";
      path
      |> List.map ~f:(fun (_, (i, j)) -> pad_lock.(i).(j))
      |> List.iter ~f:(function
          | Some n -> Printf.printf "%d\n" n
          | None -> print_string "" )
    )
