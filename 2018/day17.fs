open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

type Vein =
    | Horiz of int * (int * int)
    | Vert of int * (int * int)

let parseLine (s: string) : Vein =
    match s with
    | Regex @"x=(\d+), y=(\d+)\.\.(\d+)" [ x; y0; y1 ] ->
        Horiz (int x, (int y0, int y1))
    | Regex @"y=(\d+), x=(\d+)\.\.(\d+)" [ y; x0; x1 ] ->
        Vert (int y, (int x0, int x1))
    | _ -> sprintf "WTF: %s" s |> failwith


let input =
    System.IO.File.ReadAllLines("day17.txt")
    |> Array.map parseLine
    |> printfn "%A"

[<EntryPoint>]
let main args =
    0
