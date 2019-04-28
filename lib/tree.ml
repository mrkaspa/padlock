(* \section{Planar Tree Data Type} *)

(* A planar tree consists of nodes of tuples of an element of
   type $\alpha$ and a list of sub planar trees.  Leaf nodes
   have the empty list as their list of sub trees. *)
type 'a t = Empty | Node of 'a * ('a t list)

exception Empty_tree

(* \subsection{Planar Tree functions} *)

(* \subsubsection{Insertion and Deletion} *)

(* [create] : create an empty tree *)
let empty () = Empty

let leaf a = Node (a, [])

let node n children = Node (n, children)

(* [fold]: Fold function for trees. *)
let rec fold f = function Empty -> raise Empty_tree
                        | Node(a,lst) -> f a (List.map (fold f) lst)

(* [paths] : list of all paths from root node to other nodes in tree *)
let paths t = fold
    (fun x lst -> match lst with
       | [] -> [ [x] ]
       | y :: ys -> List.fold_left ( @ ) [[x]]
                      (List.map (fun a -> List.map (fun b-> x :: b) a) lst) ) t
