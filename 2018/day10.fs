open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then [for g in m.Groups -> g.Value] |> List.tail |> Some
    else None

type Point = {
    px: int;
    py: int;
    vx: int;
    vy: int;
}

let parseInput str =
    match str with
    | Regex @"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>" [px; py; vx; vy] ->
        { px = int px; py = int py; vx = int vx; vy = int vy }
    | _ -> failwith "WTF!"

let stepPoint p = {
    px = p.px + p.vx;
    py = p.py + p.vy;
    vx = p.vx;
    vy = p.vy;
}

let stepState ps = Array.map stepPoint ps

let height ps =
    let ys = Array.map (fun p -> p.py) ps
    (Array.max ys) - (Array.min ys) + 1

let printMessage ps =
    let ps' = Array.map (fun p -> p.px, p.py) ps |> Set.ofArray
    let xs = Set.map fst ps'
    let ys = Set.map snd ps'
    let minx = Set.minElement xs
    let maxx = Set.maxElement xs
    let miny = Set.minElement ys
    let maxy = Set.maxElement ys
    for y in [miny .. maxy] do
        for x in [minx .. maxx] do
            printf (if Set.contains (x, y) ps' then "#" else ".")
        printfn ""

[<EntryPoint>]
let main args =
    let input =
        System.IO.File.ReadAllLines("day10.txt")
        |> Array.map parseInput
    
    let rec solve ps =
        let h = height ps
        let ps' = stepState ps
        let h' = height ps'
        if h' > h then ps else solve ps'

    let messagePoints = solve input
    printMessage messagePoints

    0
