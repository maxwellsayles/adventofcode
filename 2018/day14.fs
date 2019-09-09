// fsharpc -r:FSharpx.Collections.dll day14.fs

open FSharpx.Collections

module V = PersistentVector
type V<'a> = PersistentVector<'a>

type State = {
    i: int;
    j: int;
    s: V<int>;
}

let input = 556061
let inputDigits: list<int> = [5;5;6;0;6;1]
let initState: State = {i = 0; j = 1; s = V.ofSeq [3;7]}

let step (state: State) : State =
    let i, j, s = state.i, state.j, state.s
    let x = V.nth i s
    let y = V.nth j s
    let v = x + y
    let s' = if v >= 10
             then V.conj 1 s |> V.conj (v - 10)
             else V.conj v s
    let n = V.length s'
    let i' = (i + x + 1) % n
    let j' = (j + y + 1) % n
    {i = i'; j = j'; s = s'}

let rec iterate (n: int) (s: State) : State =
    if V.length s.s < n
    then iterate n (step s)
    else s

let rec seek (n: int) (s: State): int =
    let s' = iterate (n + 6) s
    let digits = List.map (fun i -> V.nth (i + n) s'.s) [0..5]
    if inputDigits = digits
    then n
    else seek (n + 1) s'

[<EntryPoint>]
let main args =
    let scores = iterate (input + 10) initState
    let solution =
      List.map (fun i -> V.nth (i + input) scores.s) [0..9]
      |> List.map (fun d -> char(d) + '0')
      |> List.toArray
      |> fun s -> new System.String(s)
    printfn "%s" solution

    let idx = seek 0 scores
    printfn "%d" idx

    0
