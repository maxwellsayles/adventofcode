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

type Point = { x: int; y: int; }

type Edge = { s: Point; t: Point }
let edge u v =
    if u < v
    then { s = u; t = v }
    else { s = v; t = u }

let update (tails: Set<Point>) (edges: Set<Edge>) (dir: int * int) : Set<Point> * Set<Edge> =
    let tails', newEdges =
        [ for tail in tails do
          let tail' = { x = tail.x + fst dir;
                        y = tail.y + snd dir }
          yield tail', edge tail tail'
          ]
        |> List.unzip
    Set.ofList tails', Set.union edges (Set.ofList newEdges)

let findEdges (path: Path) : Set<Edge> =
    let rec pathHelper (tails: Set<Point>) (edges: Set<Edge>) (path: Path) : Set<Point> * Set<Edge> =
        match path with
        | [] -> tails, edges
        | North :: path' ->
            let tails', edges' = update tails edges (0, -1)
            pathHelper tails' edges' path'
        | East :: path' ->
            let tails', edges' = update tails edges (1, 0)
            pathHelper tails' edges' path'
        | South :: path' ->
            let tails', edges' = update tails edges (0, 1)
            pathHelper tails' edges' path'
        | West :: path' ->
            let tails', edges' = update tails edges (-1, 0)
            pathHelper tails' edges' path'
        | Branch branches :: path' ->
            let tails', edges' = updateBranches tails edges branches
            pathHelper tails' edges' path'

    and updateBranches (tails: Set<Point>) (edges: Set<Edge>) (branches: list<Path>) : Set<Point> * Set<Edge> =
        let tailss, edgess =
            List.map (pathHelper tails Set.empty) branches
            |> List.unzip
        let tails' = List.reduce Set.union tailss
        let edges' = List.fold Set.union edges edgess
        tails', edges'

    pathHelper (Set.ofList [{x = 0; y = 0}]) Set.empty path
    |> snd

[<EntryPoint>]
let main args =
    let path = tokenize input
    printfn "%d" (List.length path)

    let edges = findEdges path
    printfn "%d" (Set.count edges)

    // Find largest path with BFS 

    0
