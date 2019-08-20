// fsharpc -r:FSharpx.Collections.dll day14.fs

open FSharpx.Collections

module V = PersistentVector
type State = PersistentVector<int>

let input = 556061
let initState: State = V.ofSeq [3;7]

let step (i: int) (j: int) (s: State) : int * int * State =
    let x = V.nth i s
    let y = V.nth j s
    let v = x + y
    let s' = if v >= 10
             then V.conj 1 s |> V.conj (v - 10)
             else V.conj v s
    let n = V.length s'
    let i' = (i + x + 1) % n
    let j' = (j + y + 1) % n
    i', j', s'

let iterate (n: int) : State =
    let rec helper (n: int) (i: int) (j: int) (s: State) : State =
        if n = 0
        then s
        else let i', j', s' = step i j s
             helper (n - 1) i' j' s'
    helper n 0 1 initState

[<EntryPoint>]
let main args =
    let finalState = iterate (input + 10)
    let solution =
        List.map (fun i -> V.nth (i + input) finalState) [0..9]
        |> List.map (sprintf "%d")
        |> List.toArray
        |> System.String.Concat
    printfn "%s" solution
    0
