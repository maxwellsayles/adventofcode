// fsharpc -r:FSharpx.Collections.dll day14.fs

open FSharpx.Collections

module Q = Queue

// The record order is important so the set is ordered in reading order
type Point = { y: int; x: int }

type Grid = string []
type Path = list<Point>
type Team = Set<Point>

let (grid: Grid, initElves: Team, initGoblins: Team) =
    let inputRaw = System.IO.File.ReadAllLines("day15.txt")
    let g = Array.map (String.map (fun c -> if c = '#' then '#' else '.')) inputRaw
    let height = inputRaw.Length
    let width = inputRaw.[0].Length
    let helper f =
        [for x in [0 .. width - 1] do
         for y in [0 .. height - 1] do
         if f inputRaw.[y].[x] then yield {x = x; y = y}]
        |> Set.ofList
    let es = helper (fun c -> c = 'E')
    let gs = helper (fun c -> c = 'G')
    g, es, gs

let isAdjacent (p: Point) (ps: Team) =
    Set.contains { x = p.x - 1; y = p.y } ps ||
    Set.contains { x = p.x + 1; y = p.y } ps ||
    Set.contains { x = p.x; y = p.y - 1 } ps ||
    Set.contains { x = p.x; y = p.y + 1 } ps

let shortestPath (p: Point) (ps: Team): list<Point> =
    let rec helper (q: Queue<Path>) (visited: Set<Point>) =
        if Queue.isEmpty q
        then failwith "No path."
        else let path = Queue.head q
             let p = List.head path
             if isAdjacent p ps
             then path
             else let ps =
                      [ { x = p.x - 1; y = p.y }
                        { x = p.x + 1; y = p.y }
                        { x = p.x; y = p.y - 1 }
                        { x = p.x; y = p.y + 1 }
                        ]
                      |> List.filter (fun p -> not (Set.contains p visited))
                  let q' =
                      List.fold (fun acc p -> Queue.conj (p :: path) acc) q ps
                  let visited' = Set.union visited (Set.ofList ps)
                  helper q' visited'
    helper (Queue.ofList [[p]]) Set.empty

[<EntryPoint>]
let main args =
    printfn "%A" grid
    printfn "%A" initElves
    printfn "%A" initGoblins
    0
