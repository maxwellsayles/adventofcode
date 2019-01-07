module Seq = let rec cycle xs = seq { yield! xs; yield! cycle xs }

let rec firstRepeat (xs: seq<int>) (acc: int) (visited: Set<int>) =
    printfn "%d" acc
    if visited.Contains(acc) then acc
    else firstRepeat (Seq.tail xs) (acc + Seq.head xs) (visited.Add acc)

[<EntryPoint>]
let main args =
    let input = Seq.map int <| System.IO.File.ReadLines("day01.txt")
    printfn "%d" <| Seq.sum input

    let input =
        System.IO.File.ReadLines("day01.txt")
        |> Seq.map int
        |> Seq.toList
        |> Seq.cycle

    printfn "%d" <| firstRepeat input 0 Set.empty

    0
