// fsharpc -r:FSharpx.Collections.dll day15.fs

open FSharpx.Collections

module Q = Queue

type Team = Elves | Goblins

// The record order is important so the set is ordered in reading order
type Point = { y: int; x: int }
type Player = { y: int; x: int; team: Team; hp: int } with
    member this.Point = { x = this.x; y = this.y }

type Grid = string []
type Path = list<Point>

let initHP: int = 200

let (grid: Grid, initPlayers: Set<Player>) =
    let inputRaw = System.IO.File.ReadAllLines("day15.txt")
    let grid =
        Array.map (String.map (fun c -> if c = '#' then '#' else '.')) inputRaw
    let height = inputRaw.Length
    let width = inputRaw.[0].Length
    let helper c t =
        [for x in [0 .. width - 1] do
         for y in [0 .. height - 1] do
         if inputRaw.[y].[x] = c
         then yield {x = x; y = y; team = t; hp = initHP }]
        |> Set.ofList
    let players = Set.union (helper 'E' Elves) (helper 'G' Goblins)
    grid, players

let awayTeam = function Elves -> Goblins | Goblins -> Elves

let teamPositions (t: Team) (players: Set<Player>): Set<Point> =
    Set.filter (fun p -> p.team = t) players
    |> Set.map (fun p -> p.Point)

let isAdjacent (p: Point) (ps: Set<Point>) =
    Set.contains { x = p.x - 1; y = p.y } ps ||
    Set.contains { x = p.x + 1; y = p.y } ps ||
    Set.contains { x = p.x; y = p.y - 1 } ps ||
    Set.contains { x = p.x; y = p.y + 1 } ps

let isValidMove (ps: Set<Point>) (visited: Set<Point>) (p: Point) =
    grid.[p.y].[p.x] = '.' &&
    not (Set.contains p ps) &&
    not (Set.contains p visited)

let shortestPath (player: Player) (players: Set<Player>): list<Point> =
    let team = player.team
    let awayTeamPositions = teamPositions (awayTeam player.team) players
    let positions = Set.map (fun (p: Player) -> p.Point) players
    let rec helper (q: Queue<Path>) (visited: Set<Point>) =
        if Queue.isEmpty q
        then []
        else let path = Queue.head q
             let p = List.head path
             if isAdjacent p awayTeamPositions
             then path
             else let ps =
                      [ { x = p.x - 1; y = p.y }
                        { x = p.x + 1; y = p.y }
                        { x = p.x; y = p.y - 1 }
                        { x = p.x; y = p.y + 1 }
                        ]
                      |> List.filter (isValidMove positions visited)
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
    printfn "%A" (shortestPath { x = 12; y = 2; team = Goblins; hp = 200 } initPlayers)
    0
