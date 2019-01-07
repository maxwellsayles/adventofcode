module Seq = let rec cycle xs = seq { yield! xs; yield! cycle xs }

[<EntryPoint>]
let main args =
    let input = System.IO.File.ReadLines("day01.txt") |> Seq.map int |> Seq.toList
    let sums = Seq.scan (+) 0 input |> Seq.tail
    let sum1 = Seq.last sums
    printfn "%d" sum1

    let candidates = [ for i in sums do
                       for j in sums do
                       if i < j && (j - i) % sum1 = 0
                       then yield (i, j) ]
    List.minBy (fun (i, j) -> j - i) candidates |> snd |> printfn "%d"

    0
