// fsharpc -r:FSharpx.Collections.dll day15.fs

open FSharpx.Collections

module Q = Queue

type Team = Elves | Goblins
let awayTeam = function Elves -> Goblins | Goblins -> Elves

// The record order is important so the set is ordered in reading order
type Point = { y: int; x: int } with
    member this.Left = { x = this.x - 1; y = this.y }
    member this.Right = { x = this.x + 1; y = this.y }
    member this.Up = { x = this.x; y = this.y - 1 }
    member this.Down = { x = this.x; y = this.y + 1 }

type Player = { team: Team; hp: int } with
    member this.AwayTeam = awayTeam this.team

type Players = Map<Point, Player>

type Grid = string []
type Path = list<Point>

let defaultAP: int = 3
let initHP: int = 200

let (grid: Grid, initPlayers: Players) =
    let inputRaw = System.IO.File.ReadAllLines("day15.txt")
    let grid =
        Array.map (String.map (fun c -> if c = '#' then '#' else '.')) inputRaw
    let height = inputRaw.Length
    let width = inputRaw.[0].Length
    let helper c t =
        [for x in [0 .. width - 1] do
         for y in [0 .. height - 1] do
         if inputRaw.[y].[x] = c
         then yield { x = x; y = y }, { team = t; hp = initHP }]
    let players =
        List.append (helper 'E' Elves) (helper 'G' Goblins)
        |> Map.ofList
    grid, players

let isAdjacent (playerPos: Point) (playerTeam: Team) (players: Players) : bool =
    let awayTeam = awayTeam playerTeam
    let isAwayTeamAt (p: Point) =
        match Map.tryFind p players with
        | Some player -> player.team = awayTeam
        | _ -> false
    isAwayTeamAt playerPos.Up ||
    isAwayTeamAt playerPos.Left ||
    isAwayTeamAt playerPos.Right ||
    isAwayTeamAt playerPos.Down

let isValidMove (players: Players) (visited: Set<Point>) (p: Point) : bool =
    grid.[p.y].[p.x] = '.' &&
    not (Map.containsKey p players) &&
    not (Set.contains p visited)

let shortestPath (playerPos: Point) (playerTeam: Team) (players: Players) : list<Point> =
    let team = playerTeam
    let rec helper (q: Queue<Path>) (visited: Set<Point>) =
        if Queue.isEmpty q
        then []
        else let path: Path = Queue.head q
             let p: Point = List.head path
             if isAdjacent p team players
             then List.rev path
             else
                 // This is intentionally in reading order.
                 let ps =
                     [ p.Up; p.Left; p.Right; p.Down ]
                     |> List.filter (isValidMove players visited)
                 let q' =
                     List.fold (fun acc p -> Queue.conj (p :: path) acc)
                               (Queue.tail q) ps
                 let visited' = Set.union visited (Set.ofList ps)
                 helper q' visited'
    let q = Queue.ofList [[playerPos]]
    helper q Set.empty

let attack (playerPos: Point) (playerTeam: Team) (waiting: Players) (finished: Players) : Players * Players =
    // TODO: This needs to figure out which player to attack, and then to attack.
    // attacking decrements HP and possible removes from the set of active Players.
    waiting, finished

let stepPlayer (playerPos: Point) (player: Player) (waiting: Players) (finished: Players) : Players * Players =
    let players =
        List.append (Map.toList waiting) (Map.toList finished)
        |> Map.ofList
    let path = shortestPath playerPos player.team players
    match path with
    // No path. 
    | [] -> waiting, Map.add playerPos player finished

    // Already adjacent.
    | [p] -> attack playerPos player.team waiting finished

    // Make a move and check if the player can attack.
    | _ :: p :: _ ->
        if isAdjacent p player.team players
        then attack p player.team waiting finished
        else waiting, Map.add p player finished

let step (players: Players): Players =
    let rec helper (waiting: Players) (finished: Players) : Players =
        if Map.isEmpty waiting
        then finished
        else
            let waiting', finished' =
                let pos, player = Map.toSeq waiting |> Seq.head
                stepPlayer pos player (Map.remove pos waiting) finished
            helper waiting' finished'
    helper players Map.empty

[<EntryPoint>]
let main args =
    printfn "%A" grid
    printfn "%A" initPlayers
    printfn "%A" (shortestPath { x = 11; y = 2 } Goblins initPlayers)
    printfn "%A" (shortestPath { x = 12; y = 2 } Goblins initPlayers)
    printfn "%A" (shortestPath { x = 12; y = 12 } Goblins initPlayers)
    printfn "%A" (Seq.head initPlayers)
    0
