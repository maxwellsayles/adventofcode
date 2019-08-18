open System

type Grid = char[,]

type Turn = LeftTurn | StraightTurn | RightTurn

type CartState(x: int, y: int, dx: int, dy: int, turn: Turn) =
    member this.X = x
    member this.Y = y

    new(x: int, y: int, dx: int, dy: int) = CartState(x, y, dx, dy, LeftTurn)
    override self.ToString() = sprintf "%d %d %d %d" x y dx dy

    member this.Step(grid: Grid) = new CartState(x, y, dx, dy, turn) // TODO

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
    List.sortBy (fun (s: CartState) -> s.Y, s.X) states
    
[<EntryPoint>]
let main args =
    printfn "%d %d" width height
    printfn "%A" (orderedStates initCartStates)
    0
