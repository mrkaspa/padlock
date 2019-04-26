(* \section{Planar Tree Data Type} *)

(* A planar tree consists of nodes of tuples of an element of
   type $\alpha$ and a list of sub planar trees.  Leaf nodes
   have the empty list as their list of sub trees. *)
type 'a t = Empty | Node of 'a * ('a t list)

exception Empty_tree

(* \subsection{Planar Tree functions} *)

(* \subsubsection{Insertion and Deletion} *)

(* [create] : create an empty tree *)
let create () = Empty

let leaf a = Node (a, [])

(* [insert] : function to insert an element into a tree as the leaf of
   an existing node or leaf.   The idea is to first see if the parent node
   Node(e0,lst) exists in the tree, and if so then add the new node to
   the list of subtrees of that node *)
let rec insert e0 e = function
  | Empty -> Node(e,[])
  | (Node(a,lst) as t) ->
    if a=e0 then Node(a,lst @ [Node(e,[])])
    else match lst with
      | [] -> t
      | x :: xs -> Node(a,List.map (fun y->insert e0 e y) lst)

(* [filterNonEmpty] : remove any [Empty] trees from lists of subtrees *)
let filter_non_empty lst = List.filter (fun x->x <> Empty) lst

(* [delete] : function to remove a node (and any subnodes) from a tree.
*)
let rec delete e = function
  | Empty -> Empty
  | (Node(a,lst) as t) ->
    if a=e then Empty
    else match lst with
      | [] -> t
      | x::xs -> Node(a,filter_non_empty
                        (List.map (fun y->delete e y) lst)
                     )

(* [is_empty] : test to see if the tree is empty *)
let is_empty = function Empty -> true | _ -> false

(* \subsubsection{Map and Fold} *)

(* [fold]: Fold function for trees. *)
let rec fold f = function Empty -> raise Empty_tree
                        | Node(a,lst) -> f a (List.map (fold f) lst)

(* [map]: apply a function to each element of a tree. *)
let map f = fold (fun x lst -> Node(f x, lst))

(* [iter] : applies function f to all elements of a tree *)
let rec iter f t = match t with
  | Empty -> ()
  | Node(a,lst) -> f a; List.iter (fun x-> iter f x) lst

(* [mem] : test for membership of a tree *)
let mem e t = fold (fun x lst->List.fold_left ( || ) x lst)
    (map (fun x-> x=e) t)

(* [size] : number of elements in the tree *)
let size t = fold (fun x lst -> 1 + (List.fold_left ( + ) 0 lst)) t

let rec down n = function Empty -> Empty
                        | Node(x,lst) -> Node(n, List.map (down (n+1)) lst)

let depths t = down 1 t

(* [height] : maximum distance from root node to leaf. *)
let rec height = function Empty -> 0
                        | Node(x,lst) -> 1 + List.fold_left max 0 (List.map height lst)

(* [paths] : list of all paths from root node to other nodes in tree *)
let paths t = fold
    (fun x lst -> match lst with
       | [] -> [ [x] ]
       | y :: ys -> List.fold_left ( @ ) [[x]]
                      (List.map (fun a -> List.map (fun b-> x :: b) a) lst) ) t

(* Rather than constructing a tree from an explicit series of insert
   operations, it is easier to construct the tree by folding over a
   list of (parent node, child node) tuples.  The first element of the
   list is a bit special as the parent node value is not used by the
   insert funstion. *)

let of_list ndlst = match ndlst with
  |  [] -> Empty
  |  x :: xs ->
    let root = fst x in
    List.fold_left (fun acc (e0,e)->insert e0 e acc) (Node(root,[])) xs
