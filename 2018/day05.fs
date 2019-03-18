open System

let oppositePolarity x y =
    Char.ToLower(x) = Char.ToLower(y) && Char.IsLower(x) <> Char.IsLower(y)

let react ps =
    let rec loop acc ps =
        match ps with
        | [] -> List.rev acc
        | [x] -> loop (x::acc) []
        | x::y::ps -> if oppositePolarity x y then loop acc ps else loop (x::acc) (y::ps)
    loop [] ps

let rec reactClosure ps =
    let ps' = react ps
    if ps' = ps then ps else reactClosure ps'

[<EntryPoint>]
let main args =
    let input =
        System.IO.File.ReadAllLines("day05.txt")
        |> fun x -> x.[0]
        |> List.ofSeq

    input
    |> reactClosure
    |> List.length
    |> printfn "%d"

    0
