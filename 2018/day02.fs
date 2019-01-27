let count str =
    let hist = Seq.countBy id str
    let two = Seq.exists (fun (_, x) -> x = 2) hist
    let three = Seq.exists (fun (_, x) -> x = 3) hist
    ((if two then 1 else 0), (if three then 1 else 0))

[<EntryPoint>]
let main args =
    let input = System.IO.File.ReadLines("day02.txt")
    let (twos, threes) = Seq.map count input |> List.ofSeq |> List.unzip
    let checksum = Seq.sum twos * Seq.sum threes
    printfn "%d" checksum
    0
