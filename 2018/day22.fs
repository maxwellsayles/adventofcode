// fsharpc -r:FSharpx.Collections day22.fs && mono day22.exe

open FSharpx.Collections

type Equipment = Torch | ClimbingGear | NoEquipment
type VisitedState = { x: int; y: int; equip: Equipment }
type State = { dist: int; hist: list<VisitedState> } with
    member this.VisitedState : VisitedState =
        let hd = List.head this.hist
        { x = hd.x; y = hd.y; equip = hd.equip }

// let depth : int = 3879
// let target : int * int = 8, 713
let depth : int = 510
let target : int * int = 10, 10
let targetState : VisitedState =
    { x = fst target; y = snd target; equip = Torch }
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

let bigGridWidth : int = 2000
let bigGridHeight : int = 2000
let bigGrid : int [,] = makeGrid bigGridWidth bigGridHeight

let isValidState (s: State) : bool =
    let vs = s.VisitedState
    if vs.x < 0 || vs.y < 0 then
        false
    else
        let r = bigGrid.[vs.x, vs.y]
        match vs.equip with
        | Torch -> r <> wetRegion
        | ClimbingGear -> r <> narrowRegion
        | NoEquipment -> r <> rockyRegion

let rec search (queue: Heap<State>) (visited: Set<VisitedState>) : State * Set<VisitedState> =
    let hd = Heap.head queue
    let tl = Heap.tail queue
    let state = hd.VisitedState
    if state = targetState then
        hd, visited
    elif Set.contains state visited then
        search tl visited
    else
        let possibleStates = [
            { dist = hd.dist + 1;
              hist = { state with x = state.x - 1 } :: hd.hist }
            { dist = hd.dist + 1;
              hist = { state with x = state.x + 1 } :: hd.hist }
            { dist = hd.dist + 1;
              hist = { state with y = state.y - 1 } :: hd.hist }
            { dist = hd.dist + 1;
              hist = { state with y = state.y + 1 } :: hd.hist }
            { dist = hd.dist + 7;
              hist = { state with equip = Torch } :: hd.hist }
            { dist = hd.dist + 7;
              hist = { state with equip = ClimbingGear } :: hd.hist }
            { dist = hd.dist + 7;
              hist = { state with equip = NoEquipment } :: hd.hist }
            ]
        let states =
            List.filter isValidState possibleStates
            |> List.filter (fun (s: State) -> not (Set.contains s.VisitedState visited))
        let queue' = List.foldBack Heap.insert states tl
        let visited' = Set.add hd.VisitedState visited
        search queue' visited'

[<EntryPoint>]
let main args =
    printfn "%d" part1

    let initQueue = Heap.ofSeq false [ { dist = 0;
                                         hist = [ { x = 0; y = 0; equip = Torch } ] } ]
    let state, visited = search initQueue Set.empty
    printfn "%d" state.dist
    let maxx = Set.toList visited |> List.maxBy (fun s -> s.x) |> fun s -> s.x
    let maxy = Set.toList visited |> List.maxBy (fun s -> s.y) |> fun s -> s.y
    printfn "%d, %d" maxx maxy

    0
