
let input: char[,] =
    let inputRaw = System.IO.File.ReadAllLines("day13.txt")
    let height = Array.length inputRaw
    let width = String.length inputRaw.[0]
    Array2D.init width height (fun x y -> inputRaw.[y].[x])

[<EntryPoint>]
let main args =
    printfn "%d %d" (Array2D.length1 input) (Array2D.length2 input)
    0
