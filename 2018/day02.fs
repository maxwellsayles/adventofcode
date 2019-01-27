let count str =
    let hist = Seq.countBy id str
    let two = Seq.exists (fun (_, x) -> x = 2) hist
    let three = Seq.exists (fun (_, x) -> x = 3) hist
    ((if two then 1 else 0), (if three then 1 else 0))

let common xs ys =
    Seq.zip xs ys
    |> Seq.filter (fun (x, y) -> x = y)
    |> Seq.map fst
    |> Array.ofSeq |> (fun x -> new string(x))

[<EntryPoint>]
let main args =
    let input = System.IO.File.ReadLines("day02.txt") |> List.ofSeq
    let (twos, threes) = Seq.map count input |> List.ofSeq |> List.unzip
    let checksum = Seq.sum twos * Seq.sum threes
    printfn "%d" checksum

    let n = Seq.head input |> String.length
    seq { for x in input do for y in input do yield common x y }
    |> Seq.filter (fun (x: string) -> x.Length = n - 1)
    |> Seq.head
    |> printfn "%s"

    0
