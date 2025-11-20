[x] Make float look like float[m], use a macro.
[x] Change sample[e1,e2] to "uniform(e1,e2)"
[x] Rules without a premise should still show a horizontal line
[x] Add submoding G <= E, and corresponding subtyping rule.
[x] Have only + and times, and unary minus, as separate operators in the grammar and rules
[x] Format related work as a list mirroring the provided structure
[x] Add a gaussian sampling rule
[x] Implement the syntax of types/metas/modes with constraints
[x] Implement the type/meta/mode syntax and constraint scaffolding in OCaml
[x] In another subdir called det, put a bunch of example programs with extension filename.det.
[x] Implement 10 such examples.
[x] Implement a main driver that parses an input file and prints its pretty printed version.
[x] Implement det.sh that runs that on all files in det/
[x] Give mode variables and metas each a unique id (to be used for comparison / printing later)
[x] Rename case/of to match/with (and propagate to tex)
[x] Add a submode function and set_mode propagation for mode variables
[x] Change lambda syntax in the code to fun x => body; rec f x => body
[x] When the ocaml program processes a file, save the pretty output to a suffixed file
[x] Use a record for the constraints rather than a single constructor ADT.
[x] Implement a subtype function along similar lines as the submode function.
[x] Implement an interpreter for the language; run 100 trials and write .det.out with pretty-print and mean
[x] Add typed expressions (mutual recursion with per-subexpression type annotations)
[x] Add lists to the language with [], x::xs, and match on nil/cons.
