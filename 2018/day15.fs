// fsharpc -r:FSharpx.Collections.dll day15.fs

open FSharpx.Collections

module Q = Queue

type Team = Elves | Goblins
let awayTeam = function Elves -> Goblins | Goblins -> Elves
let teamToLetter : Team -> char = function Elves -> 'E' | Goblins -> 'G'

// The record order is important so the set is ordered in reading order
type Point = { y: int; x: int } with
    member this.Left = { x = this.x - 1; y = this.y }
    member this.Right = { x = this.x + 1; y = this.y }
    member this.Up = { x = this.x; y = this.y - 1 }
    member this.Down = { x = this.x; y = this.y + 1 }

let point (x: int) (y: int) : Point = { x = x; y = y }

type Player = { team: Team; hp: int; ap: int } with
    member this.AwayTeam = awayTeam this.team

type Players = Map<Point, Player>

type Grid = string []
type Path = list<Point>

let defaultAP: int = 3
let initHP: int = 200

let (grid: Grid, initPlayers: Players) =
    let inputFile = "day15.txt"
    let inputRaw = System.IO.File.ReadAllLines(inputFile)
    let grid =
        Array.map (String.map (fun c -> if c = '#' then '#' else '.')) inputRaw
    let height = inputRaw.Length
    let width = inputRaw.[0].Length
    let helper c t =
        [for x in [0 .. width - 1] do
         for y in [0 .. height - 1] do
         if inputRaw.[y].[x] = c
         then yield { x = x; y = y }, { team = t; hp = initHP; ap = defaultAP }]
    let players =
        List.append (helper 'E' Elves) (helper 'G' Goblins)
        |> Map.ofList
    grid, players

let gridWidth : int = String.length grid.[0]
let gridHeight : int = Array.length grid

let countTeam (players: Players) (team: Team) : int =
    Map.toList players
    |> List.map snd
    |> List.filter (fun (p: Player) -> p.team = team)
    |> List.length

let initElvesCount : int =
    countTeam initPlayers Elves

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

let attack (playerPos: Point) (waiting: Players) (players: Players) : Players * Players =
    let player = players.[playerPos]
    let isAwayTeamAt (p: Point) =
        match Map.tryFind p players with
        | Some other -> other.team = awayTeam player.team
        | _ -> false
    let attackedPos =
        [playerPos.Up; playerPos.Left; playerPos.Right; playerPos.Down]
        |> List.filter isAwayTeamAt
        |> Seq.minBy (fun (p: Point) -> players.[p].hp)

    let hp' = players.[attackedPos].hp - player.ap
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

let stepPlayer (playerPos: Point) (waiting: Players) (players: Players) : Players * Players =
    let player = players.[playerPos]
    let path = shortestPath playerPos player.team players
    match path with
    // No path. 
    | [] -> waiting, players

    // Already adjacent.
    | [_] -> attack playerPos waiting players

    // Move the player, and attack if now adjacent.
    | _ :: p :: _ ->
        let players' =
            Map.remove playerPos players
            |> Map.add p player
        if isAdjacent p player.team players'
        then attack p  waiting players'
        else waiting, players'

let step (players: Players): Players =
    let rec helper (waiting: Players) (players: Players) : Players =
        if Map.isEmpty waiting
        then players
        else
            let waiting', players' =
                let pos = Map.toSeq waiting |> Seq.head |> fst
                stepPlayer pos (Map.remove pos waiting) players
            helper waiting' players'
    helper players players

let printState (players: Players): unit =
    let rowToString (y: int): string =
        [| 0 .. gridWidth - 1 |]
        |> Array.map (fun x ->
                      match Map.tryFind (point x y) players with
                      | Some p -> teamToLetter p.team
                      | _ -> grid.[y].[x])
        |> fun arr -> new System.String(arr)

    let rowToHPString (y: int): string =
        [| 0 .. gridWidth - 1 |]
        |> Array.map (fun x ->
                      match Map.tryFind (point x y) players with
                      | Some p -> string p.hp
                      | _ -> "")
        |> Array.filter (fun s -> s <> "")
        |> fun arr -> System.String.Join(" ", arr)

    [ 0.. gridHeight - 1 ]
    |> List.iter (fun y -> printfn "%s %s" (rowToString y) (rowToHPString y))

let rec simulate (players: Players) (turnCount: int) : int * Players =
    // printfn "%d" turnCount
    // printState players
    // printfn ""
    if countTeam players Elves = 0 || countTeam players Goblins = 0
    then turnCount, players
    else simulate (step players) (turnCount + 1)

let anyElvesHurt (elvesAP: int) : option<int * Players> =
    let rec helper (players: Players) (turnCount: int) : option<int * Players> =
        let elvesCount = countTeam players Elves
        let goblinsCount = countTeam players Goblins
        if elvesCount <> initElvesCount
        then None
        elif goblinsCount = 0
        then Some(turnCount, players)
        else helper (step players) (turnCount + 1)
    let initPlayers' = Map.map (fun (p: Point) (player: Player) ->
                                if player.team = Elves
                                then {player with ap = elvesAP}
                                else player) initPlayers
    helper initPlayers' 0

let solve1 : int =
    let turnCount, finalPlayers = simulate initPlayers 0
    let hpSum = 
        Map.toList finalPlayers
        |> List.map snd
        |> List.map (fun (p: Player) -> p.hp)
        |> List.sum
    (turnCount - 1) * hpSum

let solve2 : int =
    let rec helper (elvesAP: int) =
        match anyElvesHurt elvesAP with
        | Some(turnCount, players) ->
            let hpSum =
                Map.toList players
                |> List.map snd
                |> List.map (fun (p: Player) -> p.hp)
                |> List.sum
            (turnCount - 1) * hpSum
        | None -> helper (elvesAP + 1)
    helper defaultAP

[<EntryPoint>]
let main args =
    printfn "%d" solve1
    printfn "%d" solve2
    0
