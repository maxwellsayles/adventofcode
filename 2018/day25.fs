

let input : int [][] =
    System.IO.File.ReadAllLines("day25.txt")
    |> Array.map (fun (s: string) -> s.Split([|','|]))
    |> Array.map (Array.map int)

[<EntryPoint>]
let main args =
    printfn "%A" input

    0
