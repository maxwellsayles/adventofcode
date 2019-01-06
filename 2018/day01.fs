[<EntryPoint>]
let main args =
    let input = Seq.map int <| System.IO.File.ReadLines("day01.txt")
    printfn "%d" <| Seq.sum input
    0
