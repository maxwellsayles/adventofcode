// fsharpc -r:FSharpx.Collections.dll day14.fs

open FSharpx.Collections

module Q = Queue

type Grid = string []
type Team = Set<int * int>

let (grid: Grid, initElves: Team, initGoblins: Team) =
    let inputRaw = System.IO.File.ReadAllLines("day15.txt")
    let g = Array.map (String.map (fun c -> if c = '#' then '#' else '.')) inputRaw
    let height = inputRaw.Length
    let width = inputRaw.[0].Length
    let helper f =
        [for x in [0 .. width - 1] do
         for y in [0 .. height - 1] do
         if f inputRaw.[y].[x] then yield x, y]
        |> Set.ofList
    let es = helper (fun c -> c = 'E')
    let gs = helper (fun c -> c = 'G')
    g, es, gs

let isAdjacent (x: int, y: int) (ps: Team) =
    Set.contains (x - 1, y) ps ||
    Set.contains (x + 1, y) ps ||
    Set.contains (x, y - 1) ps ||
    Set.contains (x, y + 1) ps

[<EntryPoint>]
let main args =
    printfn "%A" grid
    printfn "%A" initElves
    printfn "%A" initGoblins
    0
