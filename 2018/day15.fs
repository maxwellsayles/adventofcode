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

let attack (playerPos: Point) (playerTeam: Team) (waiting: Players) (players: Players) : Players * Players =
    let isAwayTeamAt (p: Point) =
        match Map.tryFind p players with
        | Some other -> other.team = awayTeam playerTeam
        | _ -> false
    let attackedPos =
        [playerPos.Up; playerPos.Left; playerPos.Right; playerPos.Down]
        |> List.filter isAwayTeamAt
        |> Seq.minBy (fun (p: Point) -> players.[p].hp)

    let hp' = players.[attackedPos].hp - defaultAP
    let attackedPlayer = { players.[attackedPos] with hp = hp'}

    let waiting' =
        match Map.tryFind attackedPos waiting with
        | Some other ->
            if hp' > 0
            then Map.add attackedPos attackedPlayer waiting
            else Map.remove attackedPos waiting
        | _ -> waiting

    let players' =
        if hp' > 0
        then Map.add attackedPos attackedPlayer players
        else Map.remove attackedPos players

    waiting', players'

let stepPlayer (playerPos: Point) (player: Player) (waiting: Players) (players: Players) : Players * Players =
    let path = shortestPath playerPos player.team players
    match path with
    // No path. 
    | [] -> waiting, players

    // Already adjacent.
    | [_] -> attack playerPos player.team waiting players

    // Move the player, and attack if now adjacent.
    | _ :: p :: _ ->
        let players' =
            Map.remove playerPos players
            |> Map.add p player
        if isAdjacent p player.team players'
        then attack p player.team waiting players'
        else waiting, players'

let step (players: Players): Players =
    let rec helper (waiting: Players) (players: Players) : Players =
        if Map.isEmpty waiting
        then players
        else
            let waiting', players' =
                let pos, player = Map.toSeq waiting |> Seq.head
                stepPlayer pos player (Map.remove pos waiting) players
            helper waiting' players'
    helper players players

[<EntryPoint>]
let main args =
    printfn "%A" grid
    printfn "%A" initPlayers
    printfn "%A" (shortestPath { x = 11; y = 2 } Goblins initPlayers)
    printfn "%A" (shortestPath { x = 12; y = 2 } Goblins initPlayers)
    printfn "%A" (shortestPath { x = 12; y = 12 } Goblins initPlayers)
    printfn "%A" (Seq.head initPlayers)
    0
