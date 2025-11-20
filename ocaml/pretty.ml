type doc =
  | Empty
  | Text of string
  | Line
  | Concat of doc * doc
  | Nest of int * doc
  | Union of doc * doc

let empty = Empty

let text s = if s = "" then Empty else Text s

let line = Line

let ( ^^ ) a b = Concat (a, b)

let concat docs =
  List.fold_left ( ^^ ) empty docs

let nest i d = Nest (i, d)

let rec flatten = function
  | Empty -> Empty
  | Text _ as t -> t
  | Line -> Text " "
  | Concat (a, b) -> Concat (flatten a, flatten b)
  | Nest (i, d) -> Nest (i, flatten d)
  | Union (a, _) -> flatten a

let group d = Union (flatten d, d)

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

(* Memoize best-fit computations by the (width, indent, doc_stack) state.
   The document stack is shared structurally, so physical identity is a
   good key to avoid the combinatorial explosion when many Unions are
   explored. *)
module Key = struct
  type t = int * int * (int * doc) list

  let equal (w1, k1, d1) (w2, k2, d2) = w1 = w2 && k1 = k2 && d1 == d2

  let hash (w, k, d) =
    (* Use the cons cell address of the doc list for a cheap physical hash. *)
    let addr : int = Obj.magic d in
    Hashtbl.hash (w, k, addr)
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
      match d with
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
  let rec best_memo w k (docs : (int * doc) list) =
    match Cache.find_opt cache (w, k, docs) with
    | Some ans -> ans
    | None ->
        let res =
          match docs with
          | [] -> SEmpty
          | (i, d) :: z -> (
              match d with
              | Empty -> best_memo w k z
              | Text s -> SText (s, best_memo w (k + String.length s) z)
              | Line -> SLine (i, best_memo w i z)
              | Concat (a, b) -> best_memo w k ((i, a) :: (i, b) :: z)
              | Nest (j, a) -> best_memo w k ((i + j, a) :: z)
              | Union (a, b) ->
                  let x = best_memo w k ((i, a) :: z) in
                  if fits (w - k) x then x else best_memo w k ((i, b) :: z))
        in
        Cache.replace cache (w, k, docs) res;
        res
  in
  (* Preserve original external signature, with memoized core. *)
  layout b (best_memo width 0 [ (0, doc) ]);
  Buffer.contents b

let render ?(width = 80) fmt doc =
  Format.pp_print_string fmt (to_string ~width doc)
