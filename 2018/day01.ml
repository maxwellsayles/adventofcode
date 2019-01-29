(*
I wanted to solve these using F#, but the straightforward F# version of this
puzzle performed so poorly that I was curious how the OCaml version performed.

The straightforward OCaml version performs just fine.

You need Core installed. Compile with:
> ocamlfind ocamlopt -o day01 -linkpkg -package core day01.ml -thread
*)

open Core
open Printf

module IS = Set.Make(Int)

let firstRepeat xs =
  let rec helper ys acc visited =
    if IS.mem visited acc
    then acc
    else
      match ys with
      | [] -> helper xs acc visited
      | y :: ys' -> helper ys' (acc + y) (IS.add visited acc)
  in helper xs 0 IS.empty

let () =
  let input =
    In_channel.read_lines "day01.txt"
    |> List.map ~f:int_of_string in

  List.fold_left ~init:0 ~f:(+) input |> printf "%d\n";
  firstRepeat input |> printf "%d\n";
