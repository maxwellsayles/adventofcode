
type Step =
    | North
    | East
    | South
    | West
    | Branch of Path * Path
and Path = list<Step>

let input : list<char> =
    System.IO.File.ReadAllText "day20.txt"
    |> List.ofSeq

let tokenize (cs: list<char>) : Path =
    let rec helper (acc: Path) (cs: list<char>) (t: char) : Path * list<char> =
        if List.isEmpty cs
        then List.rev acc, []
        else
            let cs' = List.tail cs
            match List.head cs with
            | 'N' -> helper (North :: acc) cs' t
            | 'E' -> helper (East :: acc) cs' t
            | 'S' -> helper (South :: acc) cs' t
            | 'W' -> helper (West :: acc) cs' t
            | '(' ->
                let p1, cs'' = helper [] cs' '|'
                let p2, cs''' = helper [] cs'' ')'
                helper (Branch (p1, p2) :: acc) cs''' t
            | '|' ->
                if t <> '|'
                then failwith <| sprintf "Expecting %c but got |. %d remains" t (List.length cs)
                else List.rev acc, cs'
            | ')' ->
                if t <> ')'
                then failwith <| sprintf "Expecting %c but got )" t
                else List.rev acc, cs'
            | '$' -> 
                if t <> '$'
                then failwith <| sprintf "Expecting %c but got $" t
                else List.rev acc, cs'
            | '^' -> helper acc cs' '$'
            | c -> failwith <| sprintf "Unrecognized: %c" c
    let p, cs' = helper [] cs '^'
    if List.isEmpty cs'
    then p
    else failwith <| sprintf "Unparsed: %A" cs'

[<EntryPoint>]
let main args =
    printfn "%d" <| List.length input
    printfn "%A" <| tokenize input
    0
