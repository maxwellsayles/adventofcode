
[<EntryPoint>]
let main args =
    let input =
        System.IO.File.ReadAllLines("day13.txt")
        |> Array.map Array.ofSeq
    printfn "%d %d" (Array.length input) (Array.length input.[0])
    0
