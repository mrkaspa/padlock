open Lib;
open Core;

let pad_lock = [|
  [|None, Some(2), Some(3)|],
  [|Some(4), None, Some(6)|],
  [|Some(7), Some(8), Some(9)|],
|];

let moves = [
  (2, 1),
  ((-2), 1),
  (2, (-1)),
  ((-2), (-1)),
  (1, 2),
  (1, (-2)),
  ((-1), 2),
  ((-1), (-2)),
];

module Tuple_map =
  Map.Make({
    type t = (int, int);
    include Tuple.Comparable(Int, Int);
  });

let pos_solutions = ((i, j) as idx, cache: Tuple_map.t(list((int, int)))) =>
  switch (Tuple_map.find(cache, idx)) {
  | Some(sols) => (sols, cache)
  | None =>
    let sols =
      List.fold_right(
        ~f=
          ((im, jm), ls) => {
            let inw = i + im;
            let jnw = j + jm;
            if (inw >= 0 && inw < 3 && jnw >= 0 && jnw < 3) {
              switch (pad_lock[inw][jnw]) {
              | Some(_) => [(inw, jnw), ...ls]
              | None => ls
              };
            } else {
              ls;
            };
          },
        ~init=[],
        moves,
      );

    (sols, Tuple_map.add_exn(cache, ~key=idx, ~data=sols));
  };

let rec find_n_combinations =
        (
          (i, j) as idx,
          depth,
          max_depth,
          ~cache: Tuple_map.t(list((int, int))),
        ) =>
  if (depth > max_depth) {
    switch (pad_lock[i][j]) {
    | Some(_) => Tree.leaf((depth, idx))
    | None => Empty
    };
  } else {
    let (pos, cache) = pos_solutions(idx, cache);
    let children =
      List.fold_right(
        ~f=
          (elem, nodes) => {
            let child =
              find_n_combinations(elem, depth + 1, max_depth, ~cache);
            switch (child) {
            | Node(_) as inner => [inner, ...nodes]
            | _ => nodes
            };
          },
        ~init=[],
        pos,
      );

    Tree.node((depth, idx), children);
  };

let () =
  find_n_combinations((0, 2), 0, 4, Tuple_map.empty)
  |> Tree.paths
  |> List.filter(~f=ls => List.length(ls) == 4)
  |> List.iter(~f=path => {
       print_string("Path >>\n");
       path
       |> List.map(~f=((_, (i, j))) => pad_lock[i][j])
       |> List.iter(
            ~f=
              fun
              | Some(n) => Printf.printf("%d\n", n)
              | None => print_string(""),
          );
     });
