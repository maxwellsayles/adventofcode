type Step =
    | North
    | East
    | South
    | West
    | Branch of list<Path>
and Path = list<Step>

let input : list<char> =
    System.IO.File.ReadAllText "day20.txt"
    |> List.ofSeq

let tokenize (cs: list<char>) : Path =
    let rec parsePath (acc: Path) (cs: list<char>) : Path * list<char> =
        if List.isEmpty cs
        then List.rev acc, []
        else
            let cs' = List.tail cs
            match List.head cs with
            | 'N' -> parsePath (North :: acc) cs'
            | 'E' -> parsePath (East :: acc) cs'
            | 'S' -> parsePath (South :: acc) cs'
            | 'W' -> parsePath (West :: acc) cs'
            | '(' ->
                let ps, cs'' = parseBranch [] cs'
                parsePath (Branch ps :: acc) cs''
            | '|' | ')' ->
                // Return the path and the remainder, including the terminal
                // character to be used in determining whether to continue
                // branching.
                List.rev acc, cs
            | '^' | '$' | '\010' ->
                parsePath acc cs'
            | c -> failwith <| sprintf "Unexpected %c" c

    and parseBranch (ps: list<Path>) (cs: list<char>) : list<Path> * list<char> =
        let p, cs' = parsePath [] cs
        let ps' = p :: ps
        let cs'' = List.tail cs'
        match List.head cs' with
        | '|' -> parseBranch ps' cs''
        | ')' -> List.rev ps', cs''
        | c -> failwith <| sprintf "Unexpected %c" c

    let p, cs' = parsePath [] cs
    if List.isEmpty cs'
    then p
    else failwith <| sprintf "Unparsed: %A" cs'

type Point = { x: int; y: int; } with
    member this.North = { x = this.x; y = this.y - 1 }
    member this.East = { x = this.x + 1; y = this.y }
    member this.South = { x = this.x; y = this.y + 1 }
    member this.West = { x = this.x - 1; y = this.y }

let findRooms (path: Path) : Set<Point> =
    let rec helper (current: Point) (acc: Set<Point>) (path: Path) : Set<Point> =
        let acc' = Set.add current acc
        match path with
        | [] -> acc
        | North :: path' -> helper current.North acc' path'
        | East :: path' -> helper current.East acc' path'
        | South :: path' -> helper current.South acc' path'
        | West :: path' -> helper current.West acc' path'
        | Branch paths :: path' -> List.fold (helper current) acc' paths
    let start = { x = 0; y = 0 }
    helper start Set.empty path

[<EntryPoint>]
let main args =
    let path = tokenize input
    let rooms = findRooms path
    printfn "%d" <| Set.count rooms

    // Find largest path with BFS 

    0
