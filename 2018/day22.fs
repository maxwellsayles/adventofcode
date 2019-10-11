// fsharpc -r:FSharpx.Collections day22.fs && mono day22.exe

open FSharpx.Collections

let depth : int = 3879
let target : int * int = 8, 713
// let depth : int = 510
// let target : int * int = 10, 10
let gridWidth : int = fst target + 1
let gridHeight : int = snd target + 1

let makeGrid (width: int) (height: int) : int [,] =
    let res = Array2D.create width height 0
    let es = Array2D.create width height 0
    let fillTerrain (x: int) (y: int) =
        let geoIdx =
            if x = 0 && y = 0 then
                0
            elif x = fst target && y = snd target then
                0
            elif x = 0 then
                y * 48271
            elif y = 0 then
                x * 16807
            else
                es.[x, y - 1] * es.[x - 1, y]
        let errosion = (geoIdx + depth) % 20183
        es.[x, y] <- errosion
        res.[x, y] <- errosion % 3
    for y in [0..snd target] do
        for x in [0..fst target] do
            fillTerrain x y
    res

let part1 : int =
    let grid : int [,] = makeGrid gridWidth gridHeight
    let mutable sum = 0
    Array2D.iter (fun v -> sum <- sum + v) grid
    sum

let rockyRegion : int = 0
let wetRegion : int = 1
let narrowRegion : int = 2

type Equipment = Torch | ClimbingGear | NoEquipment
type VisitedState = int * int * Equipment
type State =
    { dist: int
      x: int
      y: int
      equip: Equipment
    } with
    member this.VisitedState : VisitedState = this.x, this.y, this.equip
    member this.Pos : int * int = this.x, this.y

let bigGridWidth : int = 2000
let bigGridHeight : int = 2000
let bigGrid : int [,] = makeGrid bigGridWidth bigGridHeight

let isValidState (s: State) : bool =
    if s.x < 0 || s.y < 0 then
        false
    else
        match bigGrid.[s.x, s.y] with
        | r when r = rockyRegion -> s.equip <> NoEquipment
        | r when r = wetRegion -> s.equip <> Torch
        | r when r = narrowRegion -> s.equip <> ClimbingGear
        | _ -> failwith "WTF!"

let rec search (queue: Heap<State>) (visited: Set<VisitedState>) : int * Set<VisitedState> =
    let hd = Heap.head queue
    let tl = Heap.tail queue
    if hd.Pos = target && hd.equip = Torch then
        hd.dist, visited
    elif Set.contains hd.VisitedState visited then
        search tl visited
    else
        let s = { hd with dist = hd.dist + 1 }
        let s' = { hd with dist = hd.dist + 7 }
        let possibleStates = [
            { s with x = s.x - 1 }
            { s with x = s.x + 1 }
            { s with y = s.y - 1 }
            { s with y = s.y + 1 }
            { s' with equip = Torch }
            { s' with equip = ClimbingGear }
            { s' with equip = NoEquipment } ]
        let states = List.filter isValidState possibleStates
        let queue' = List.foldBack Heap.insert states tl
        let visited' = Set.add hd.VisitedState visited
        search queue' visited'

[<EntryPoint>]
let main args =
    printfn "%d" part1

    let initQueue = Heap.ofSeq false [ { dist = 0; x = 0; y = 0; equip = Torch } ]
    let dist, visited = search initQueue Set.empty
    printfn "%d" dist
    let maxx = Set.toList visited |> List.maxBy (fun (x, _, _) -> x) |> fun (x, _, _) -> x
    let maxy = Set.toList visited |> List.maxBy (fun (_, y, _) -> y) |> fun (_, y, _) -> y
    printfn "%d, %d" maxx maxy

    0
