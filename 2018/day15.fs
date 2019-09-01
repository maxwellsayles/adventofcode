// fsharpc -r:FSharpx.Collections.dll day15.fs

open FSharpx.Collections

module Q = Queue

type Team = Elves | Goblins

// The record order is important so the set is ordered in reading order
type Point = { y: int; x: int }
type Player = { y: int; x: int; team: Team } with
    member this.Point = { x = this.x; y = this.y }

type Grid = string []
type Path = list<Point>

let (grid: Grid, initPlayers: Set<Player>) =
    let inputRaw = System.IO.File.ReadAllLines("day15.txt")
    let grid =
        Array.map (String.map (fun c -> if c = '#' then '#' else '.')) inputRaw
    let height = inputRaw.Length
    let width = inputRaw.[0].Length
    let helper c t =
        [for x in [0 .. width - 1] do
         for y in [0 .. height - 1] do
         if inputRaw.[y].[x] = c then yield {x = x; y = y; team = t}]
        |> Set.ofList
    let players = Set.union (helper 'E' Elves) (helper 'G' Goblins)
    grid, players

let otherTeam = function Elves -> Goblins | Goblins -> Elves

let isAdjacent (p: Player) (ps: Set<Player>) =
    let t = otherTeam p.team
    Set.contains { x = p.x - 1; y = p.y; team = t } ps ||
    Set.contains { x = p.x + 1; y = p.y; team = t } ps ||
    Set.contains { x = p.x; y = p.y - 1; team = t } ps ||
    Set.contains { x = p.x; y = p.y + 1; team = t } ps

let isValidMove (players: Set<Player>) (visited: Set<Point>) (p: Point) =
    grid.[p.y].[p.x] = '.' &&
    not (Set.contains { x = p.x; y = p.y; team = Elves } players ||
         Set.contains { x = p.x; y = p.y; team = Goblins } players) &&
    not (Set.contains p visited)

let shortestPath (player: Player) (players: Set<Player>): list<Point> =
    let team = player.team
    let rec helper (q: Queue<Path>) (visited: Set<Point>) =
        if Queue.isEmpty q
        then []
        else let path = Queue.head q
             let p = List.head path
             if isAdjacent { x = p.x; y = p.y; team = team } players
             then path
             else let ps =
                      [ { x = p.x - 1; y = p.y }
                        { x = p.x + 1; y = p.y }
                        { x = p.x; y = p.y - 1 }
                        { x = p.x; y = p.y + 1 }
                        ]
                      |> List.filter (isValidMove players visited)
                  let q' =
                      List.fold (fun acc p -> Queue.conj (p :: path) acc)
                                (Queue.tail q) ps
                  let visited' = Set.union visited (Set.ofList ps)
                  helper q' visited'
    let q = Queue.ofList [[player.Point]]
    helper q Set.empty

[<EntryPoint>]
let main args =
    printfn "%A" grid
    printfn "%A" initPlayers
    printfn "%A" (shortestPath (Set.minElement initPlayers) initPlayers)
    printfn "%A" (shortestPath { x = 12; y = 2; team = Goblins } initPlayers)
    0
