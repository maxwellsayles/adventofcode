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

let makeGrid (width: int) (height: int) : int [,] =
    let es = Array2D.create width height 0
    let fillTerrain (x: int) (y: int) =
        let geoIdx =
            if x = 0 && y = 0 then
                0
            elif x = fst target && y = snd target then
                0
            elif y = 0 then
                x * 16807
            elif x = 0 then
                y * 48271
            else
                es.[x, y - 1] * es.[x - 1, y]
        let errosion = (geoIdx + depth) % 20183
        es.[x, y] <- errosion

    for y in [0..snd target] do
        for x in [0..fst target] do
            fillTerrain x y

    Array2D.init width height (fun x y -> es.[x, y] % 3)

let regionToChar : char [] = [| '.'; '='; '|' |]

let printGrid (width: int) (height: int) (grid: int [,]) (path: list<VisitedState>) : unit =
    let coordToEquip =
        List.map (fun vs -> (vs.x, vs.y), vs.equip) path
        |> Map.ofList
    for y in [0 .. height - 1] do
        for x in [0 .. width - 1] do
            match Map.tryFind (x, y) coordToEquip with
            | Some Torch -> printf "T"
            | Some ClimbingGear -> printf "C"
            | Some NoEquipment -> printf "N"
            | None -> printf "%c" regionToChar.[grid.[x, y]]
        printfn ""

let part1 : int =
    let grid : int [,] = makeGrid (fst target + 1) (snd target + 1)
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
        { hd with hist = List.rev hd.hist }, visited
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

    printGrid 16 16 bigGrid [] // state.hist

    0
