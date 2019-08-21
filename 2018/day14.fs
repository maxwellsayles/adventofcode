// fsharpc -r:FSharpx.Collections.dll day14.fs

open FSharpx.Collections

module V = PersistentVector
type State = PersistentVector<int>

let input = 556061
let inputDigits = [5;5;6;0;6;1]
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

let scores: seq<int> =
    let rec helper (n: int) (i: int) (j: int) (s: State) : seq<int> =
        if V.length s > n
        then seq { yield (V.nth n s); yield! helper (n + 1) i j s }
        else let i', j', s' = step i j s
             helper n i' j' s'
    seq { yield! helper 0 0 1 initState }

let rec seek (n: int) (s: list<int>): int =
    if List.isEmpty s
    then -1
    else let digits = List.take 6 s
         if inputDigits = digits then n else seek (n + 1) (List.tail s)

[<EntryPoint>]
let main args =
    let solution =
        Seq.skip input scores
        |> Seq.take 10
        |> List.ofSeq
        |> List.map (fun d -> char(d) + '0')
        |> List.toArray
        |> fun s -> new System.String(s)
    printfn "%s" solution

    let samples = Seq.take 30000000 scores |> List.ofSeq
    let idx = seek 0 samples
    printfn "%d" idx

    0
