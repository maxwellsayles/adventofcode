open System

type Grid = char[,]

type Turn = LeftTurn | StraightTurn | RightTurn

let input: Grid =
    let inputRaw = System.IO.File.ReadAllLines("day13.txt")
    let height = Array.length inputRaw
    let width = String.length inputRaw.[0]
    Array2D.init width height (fun x y -> inputRaw.[y].[x])

let width = Array2D.length1 input
let height = Array2D.length2 input

let grid: Grid =
    Array2D.map (function
                 | '>' | '<' -> '-'
                 | '^' | 'v' -> '|'
                 | c -> c
                 ) input

type CartState(x: int, y: int, dx: int, dy: int, turn: Turn) =
    new(x: int, y: int, dx: int, dy: int) = CartState(x, y, dx, dy, LeftTurn)
    override this.ToString() = sprintf "%d %d %d %d" x y dx dy

    member this.X = x
    member this.Y = y
    member private this.Cell = grid.[x, y]

    member private this.Forward: CartState =
        new CartState(x + dx, y + dy, dx, dy, turn)

    member private this.NextTurn: CartState =
        let turn' =
            match turn with
            | LeftTurn -> StraightTurn
            | StraightTurn -> RightTurn
            | RightTurn -> LeftTurn
        new CartState(x, y, dx, dy, turn')

    member private this.Intersection: CartState =
        match turn with
        | LeftTurn -> this.TurnLeft.NextTurn
        | StraightTurn -> this.NextTurn
        | RightTurn -> this.TurnRight.NextTurn

    member private this.TurnLeft: CartState =
        new CartState(x, y, dy, -dx, turn)

    member private this.TurnRight: CartState =
        new CartState(x, y, -dy, dx, turn)

    member this.Step =
        let p = this.Forward
        match p.Cell, dx, dy with
        | '/', 0, _ -> p.TurnRight
        | '/', _, 0 -> p.TurnLeft
        | '\\', 0, _ -> p.TurnLeft
        | '\\', _, 0 -> p.TurnRight
        | '+', _, _ -> p.Intersection
        | '-', _, _ | '|', _, _ -> p
        | _ -> failwith "WTF!"

let initCartStates: list<CartState> =
    let mutable states = List.empty
    Array2D.iteri (fun x y -> function
                   | '>' -> states <- new CartState(x, y, 1, 0) :: states
                   | '<' -> states <- new CartState(x, y, -1, 0) :: states
                   | '^' -> states <- new CartState(x, y, 0, -1) :: states 
                   | 'v' -> states <- new CartState(x, y, 0, 1) :: states
                   | _ -> ()
                   ) input
    states

let orderedStates (states: list<CartState>): list<CartState> =
    List.sortBy (fun (state: CartState) -> state.Y, state.X) states

let collisions (states: list<CartState>): seq<int * int> =
    Seq.countBy (fun (state: CartState) -> state.X, state.Y) states
    |> Seq.filter (fun (k, v) -> v > 1)
    |> Seq.map fst

let runTillCollision (states: list<CartState>): seq<int * int> =
    let rec helper (states: list<CartState>) (acc: list<CartState>) : seq<int * int> =
        let cs = collisions (List.append states acc)
        if not (Seq.isEmpty cs)
        then cs
        elif List.isEmpty states
        then helper (orderedStates acc) []
        else let states' = List.tail states
             let state' = (List.head states).Step
             helper states' (state' :: acc)
    helper states []

[<EntryPoint>]
let main args =
    printfn "%A" (runTillCollision initCartStates)
    0
