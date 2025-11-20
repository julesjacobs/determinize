type doc_node =
  | Empty
  | Text of string
  | Line
  | Concat of doc * doc
  | Nest of int * doc
  | Union of doc * doc

and doc = {
  id : int;
  node : doc_node;
}

let doc_counter = ref 0

let mk node =
  incr doc_counter;
  { id = !doc_counter; node }

let empty = mk Empty

let text s = if s = "" then empty else mk (Text s)

let line = mk Line

let ( ^^ ) a b = mk (Concat (a, b))

let concat docs =
  List.fold_left ( ^^ ) empty docs

let nest i d = mk (Nest (i, d))

let rec flatten d =
  match d.node with
  | Empty -> empty
  | Text _ -> d
  | Line -> text " "
  | Concat (a, b) -> mk (Concat (flatten a, flatten b))
  | Nest (i, d') -> mk (Nest (i, flatten d'))
  | Union (a, _) -> flatten a

let group d = mk (Union (flatten d, d))

let softline = group line
let softbreak = softline

let space = text " "

let hsep docs =
  match docs with
  | [] -> empty
  | d :: ds -> group (List.fold_left (fun acc x -> acc ^^ space ^^ x) d ds)

let sep docs =
  match docs with
  | [] -> empty
  | d :: ds -> group (List.fold_left (fun acc x -> acc ^^ softline ^^ x) d ds)

let vsep docs =
  match docs with
  | [] -> empty
  | d :: ds -> List.fold_left (fun acc x -> acc ^^ line ^^ x) d ds

let join sep docs =
  match docs with
  | [] -> empty
  | d :: ds -> List.fold_left (fun acc x -> acc ^^ sep ^^ x) d ds

let enclose l r d =
  group (text l ^^ nest 2 (softline ^^ d) ^^ softline ^^ text r)

let parens d = enclose "(" ")" d
let angles d = enclose "<" ">" d

let brackets d = enclose "[" "]" d

let enclose_separated l r sep docs =
  match docs with
  | [] -> text (l ^ r)
  | _ -> enclose l r (join sep docs)

let punctuate sep docs =
  let rec go acc = function
    | [] -> List.rev acc
    | [ d ] -> List.rev (d :: acc)
    | d :: ds -> go ((sep ^^ d) :: acc) ds
  in
  go [] docs

(* Stream of rendered primitives after layout decision. *)
type simple =
  | SEmpty
  | SText of string * simple
  | SLine of int * simple

(* Memoization keys rely on stable ids on doc nodes. *)
module Key = struct
  type t = int * int * (int * int) list

  let equal (w1, k1, ds1) (w2, k2, ds2) = w1 = w2 && k1 = k2 && ds1 = ds2

  let hash (w, k, ds) = Hashtbl.hash (w, k, ds)
end

module Cache = Hashtbl.Make (Key)

let cache : simple Cache.t = Cache.create 4096

let rec fits w doc =
  if w < 0 then
    false
  else
    match doc with
    | SEmpty -> true
    | SLine _ -> true
    | SText (s, d) -> fits (w - String.length s) d

let rec best w k docs =
  match docs with
  | [] -> SEmpty
  | (i, d) :: z -> (
      match d.node with
      | Empty -> best w k z
      | Text s -> SText (s, best w (k + String.length s) z)
      | Line -> SLine (i, best w i z)
      | Concat (a, b) -> best w k ((i, a) :: (i, b) :: z)
      | Nest (j, a) -> best w k ((i + j, a) :: z)
      | Union (a, b) ->
          let x = best w k ((i, a) :: z) in
          if fits (w - k) x then x else best w k ((i, b) :: z))

let rec layout buf = function
  | SEmpty -> ()
  | SText (s, d) ->
      Buffer.add_string buf s;
      layout buf d
  | SLine (i, d) ->
      Buffer.add_char buf '\n';
      Buffer.add_string buf (String.make i ' ');
      layout buf d

let to_string ?(width = 80) doc =
  Cache.clear cache;
  let b = Buffer.create 256 in
  let rec best_memo w k docs =
    let key = (w, k, List.map (fun (i, d) -> (i, d.id)) docs) in
    match Cache.find_opt cache key with
    | Some ans -> ans
    | None ->
        let res =
          match docs with
          | [] -> SEmpty
          | (i, d) :: z -> (
              match d.node with
              | Empty -> best_memo w k z
              | Text s -> SText (s, best_memo w (k + String.length s) z)
              | Line -> SLine (i, best_memo w i z)
              | Concat (a, b) -> best_memo w k ((i, a) :: (i, b) :: z)
              | Nest (j, a) -> best_memo w k ((i + j, a) :: z)
              | Union (a, b) ->
                  let x = best_memo w k ((i, a) :: z) in
                  if fits (w - k) x then x else best_memo w k ((i, b) :: z))
        in
        Cache.replace cache key res;
        res
  in
  layout b (best_memo width 0 [ (0, doc) ]);
  Buffer.contents b

let render ?(width = 80) fmt doc =
  Format.pp_print_string fmt (to_string ~width doc)
