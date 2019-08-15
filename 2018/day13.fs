open System

type Map = char[,]

type Turn = LeftTurn | StraightTurn | RightTurn

[<CustomComparison; CustomEquality; Struct>]
type CartState(x: int, y: int, dx: int, dy: int, turn: Turn) =
    member this.X = x
    member this.Y = y

    new(x: int, y: int, dx: int, dy: int) = CartState(x, y, dx, dy, LeftTurn)
    override self.ToString() = sprintf "%d %d %d %d" x y dx dy

    member private this.Compare (that: CartState) = 
        if this.Y < that.Y then -1
            elif this.Y > that.Y then 1
            elif this.X < that.X then -1
            elif this.X > that.X then 1
            else 0

    interface IComparable<CartState> with
        member this.CompareTo that = this.Compare that

    interface IComparable with
        member this.CompareTo that =
            match that with
              | :? CartState as that' -> this.Compare that'
              | _ -> invalidArg "obj" "not a Category"

    override this.Equals(that) =
        match that with
            | :? CartState as that' -> this.X = that'.X && this.Y = that'.Y
            | _ -> false

    interface IEquatable<CartState> with
        member this.Equals(that: CartState) =
            this.Equals(that)

    override this.GetHashCode() = (this.X, this.Y).GetHashCode()

    member this.Step(map: Map) = new CartState(x, y, dx, dy, turn) // TODO


let input: Map =
    let inputRaw = System.IO.File.ReadAllLines("day13.txt")
    let height = Array.length inputRaw
    let width = String.length inputRaw.[0]
    Array2D.init width height (fun x y -> inputRaw.[y].[x])

let width = Array2D.length1 input
let height = Array2D.length2 input

let map: Map =
    Array2D.map (function
                 | '>' | '<' -> '-'
                 | '^' | 'v' -> '|'
                 | c -> c
                 ) input

let initCartStates: Set<CartState> =
    let mutable states = Set.empty
    Array2D.iteri (fun x y -> function
                   | '>' -> states <- Set.add (new CartState(x, y, 1, 0)) states
                   | '<' -> states <- Set.add (new CartState(x, y, -1, 0)) states
                   | '^' -> states <- Set.add (new CartState(x, y, 0, -1)) states 
                   | 'v' -> states <- Set.add (new CartState(x, y, 0, 1)) states
                   | _ -> ()
                   ) input
    states
    
[<EntryPoint>]
let main args =
    printfn "%d %d" width height
    printfn "%A" initCartStates
    printfn "%d" (Set.count initCartStates)
    0
