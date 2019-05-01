type State = {
    zeros: Set<char>;
    inDegree: Map<char, int>;
}

let headNode (state: State): char = state.zeros.MinimumElement

let tailState (adj: Map<char, Set<char>>) (state: State): State =
    let hd = headNode state
    let inDegree' =
        Set.fold (fun (acc: Map<char, int>) y -> acc.Add(y, acc.[y] - 1)) state.inDegree adj.[hd]
    let zeros' = Set.union (state.zeros.Remove hd)
                           (Map.toSeq inDegree' |> Seq.filter (fun (_, c) -> c = 0) |> Seq.map fst |> Set.ofSeq)

    { zeros = zeros'; inDegree = inDegree' }

let initStateFromEdges (edges: seq<char * char>): State =
    let inDegree =
        Seq.fold (fun (acc: Map<char, int>) (_, y) ->
                  if acc.ContainsKey y
                  then acc.Add(y, acc.[y] + 1)
                  else acc.Add(y, 1)) Map.empty edges

    let zeros =
        Set.difference (Seq.map fst edges |> Set.ofSeq) (Seq.map snd edges |> Set.ofSeq)

    { zeros = zeros; inDegree = inDegree }

let isFinished (state: State): bool = state.zeros.IsEmpty

[<EntryPoint>]
let main args =
    let edges =
        System.IO.File.ReadAllLines("day07.txt")
        |> Array.map (fun s -> s.[5], s.[36])

    let outNodes = Seq.map fst edges |> Set.ofSeq
    let inNodes = Seq.map snd edges |> Set.ofSeq

    let adj =
        Array.fold (fun (acc: Map<char, Set<char>>) (x, y) -> acc.Add(x, acc.[x].Add(y)))
                   (Seq.map (fun n -> n, Set.empty) outNodes |> Map.ofSeq)
                   edges

    let initState = initStateFromEdges edges

    printfn "%A" (headNode initState)

    0
