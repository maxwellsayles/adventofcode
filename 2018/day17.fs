open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

type Vein =
    | Horiz of int * (int * int)
    | Vert of int * (int * int)

let tokenizeLine (s: string) : Vein =
    match s with
    | Regex @"x=(\d+), y=(\d+)\.\.(\d+)" [ x; y0; y1 ] ->
        Horiz (int x, (int y0, int y1))
    | Regex @"y=(\d+), x=(\d+)\.\.(\d+)" [ y; x0; x1 ] ->
        Vert (int y, (int x0, int x1))
    | _ -> sprintf "WTF: %s" s |> failwith

type CellValue = Sand | Clay | FlowingWater | TrappedWater

type GameBoard = Map<int * int, CellValue>

let rec expandVein (board: GameBoard) (vein: Vein) : GameBoard =
    match vein with
    | Horiz (x, (y0, y1)) ->
        let board' = Map.add (x, y0) Clay board
        if y0 = y1
        then board'
        else expandVein board' (Horiz (x, (y0 + 1, y1)))
    | Vert (y, (x0, x1)) ->
        let board' = Map.add (x0, y) Clay board
        if x0 = x1
        then board'
        else expandVein board' (Vert (y, (x0 + 1, x1)))

let board : GameBoard =
    System.IO.File.ReadAllLines("day17.txt")
    |> Array.map tokenizeLine
    |> Array.fold expandVein Map.empty

let boardMinY : int = Map.toSeq board |> Seq.map (fst >> snd) |> Seq.min
let boardMaxY : int = Map.toSeq board |> Seq.map (fst >> snd) |> Seq.max

let springStartX : int = 500
let springStartY : int = 0

let boardLookup (x: int) (y: int) (board: GameBoard) : CellValue =
    match Map.tryFind (x, y) board with
    | Some c -> c
    | _ -> Sand

let cellToChar : CellValue -> char = function
    | Sand -> '.'
    | Clay -> '#'
    | FlowingWater -> '|'
    | TrappedWater -> '~'

let renderBoard (board: GameBoard) : unit =
    let minX : int = Map.toSeq board |> Seq.map (fst >> fst) |> Seq.min
    let maxX : int = Map.toSeq board |> Seq.map (fst >> fst) |> Seq.max
    let minY : int = Map.toSeq board |> Seq.map (fst >> snd) |> Seq.min
    let maxY : int = Map.toSeq board |> Seq.map (fst >> snd) |> Seq.max
    for y in [minY .. maxY] do
        for x in [minX .. maxX] do
            boardLookup x y board
            |> cellToChar
            |> printf "%c" 
        printfn ""
    printfn ""

let addToBoard (p: int * int) (v: CellValue) (board: GameBoard) : GameBoard =
    Map.add p v board
    // let board' = Map.add p v board
    // renderBoard board'
    // board'

let isRowContained (x: int) (y: int) (board: GameBoard) : bool =
    let rec isContained (x': int) (dx: int) : bool =
        let v = boardLookup x' y board
        let v' = boardLookup x' (y + 1) board
        if v = Clay || v = TrappedWater
        then true
        elif v' = Clay || v' = TrappedWater
        then isContained (x' + dx) dx
        else false
    isContained x -1 && isContained x +1

let floodRow (x: int) (y: int) (board: GameBoard) : GameBoard =
    let rec flood (x': int) (dx: int) (board: GameBoard) : GameBoard =
        if boardLookup x' y board = Clay
        then board
        else
            let board' = addToBoard (x', y) TrappedWater board
            flood (x' + dx) dx board'
    board
    |> flood x -1
    |> flood x +1

let rec flood (x: int) (y: int) (board: GameBoard) : GameBoard =
    let v = boardLookup x y board
    if y > boardMaxY || v <> Sand
    then board
    else
        let board' =
            addToBoard (x, y) FlowingWater board
            |> flood x (y + 1)
        let v' = boardLookup x (y + 1) board'
        if isRowContained x y board'
        then floodRow x y board'
        elif v' = Clay || v' = TrappedWater
        then board' |> flood (x - 1) y |> flood (x + 1) y
        else board'

[<EntryPoint>]
let main args =
    let finalBoard = flood springStartX springStartY board
    let solve1 =
        finalBoard
        |> Map.toSeq
        |> Seq.filter (fun ((_, y), _) -> y >= boardMinY && y <= boardMaxY)
        |> Seq.filter (fun (_, v) -> v = FlowingWater || v = TrappedWater)
        |> Seq.length
    printfn "%d" solve1
    0
