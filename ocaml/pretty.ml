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
  let b = Buffer.create 256 in
  layout b (best width 0 [ (0, doc) ]);
  Buffer.contents b

let render ?(width = 80) fmt doc =
  Format.pp_print_string fmt (to_string ~width doc)
