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

type GameBoard = Map<int * int, char>

let rec expandVein (board: GameBoard) (vein: Vein) : GameBoard =
    match vein with
    | Horiz (x, (y0, y1)) ->
        let board' = Map.add (x, y0) '#' board
        if y0 = y1
        then board'
        else expandVein board' (Horiz (x, (y0 + 1, y1)))
    | Vert (y, (x0, x1)) ->
        let board' = Map.add (x0, y) '#' board
        if x0 = x1
        then board'
        else expandVein board' (Vert (y, (x0 + 1, x1)))

let input : GameBoard =
    System.IO.File.ReadAllLines("day17.txt")
    |> Array.map tokenizeLine
    |> Array.fold expandVein Map.empty

let inputMinY : int = Map.toSeq input |> Seq.map (fst >> snd) |> Seq.min
let inputMaxY : int = Map.toSeq input |> Seq.map (fst >> snd) |> Seq.max

[<EntryPoint>]
let main args =
    printfn "%A" input
    printfn "%d" inputMinY
    printfn "%d" inputMaxY
    0
