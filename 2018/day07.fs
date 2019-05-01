type State = {
    zeros: Set<char>;
    counts: Map<char, int>;
}

let headNode (state: State): char = state.zeros.MinimumElement

let tailState (adj: Map<char, Set<char>>) (state: State): State =
    let hd = headNode state
    let counts' =
        Set.fold (fun (acc: Map<char, int>) y -> acc.Add(y, acc.[y] - 1)) state.counts adj.[hd]
    let zeros' = Set.union (state.zeros.Remove hd)
                           (Map.toSeq counts' |> Seq.filter (fun (_, c) -> c = 0) |> Seq.map fst |> Set.ofSeq)

    { zeros = zeros'; counts = counts' }

let initStateFromEdges (edges: seq<char * char>): State =
    let counts =
        Seq.fold (fun (acc: Map<char, int>) (_, y) ->
                  if acc.ContainsKey y
                  then acc.Add(y, acc.[y] + 1)
                  else acc.Add(y, 1)) Map.empty edges

    let zeros =
        Set.difference (Seq.map fst edges |> Set.ofSeq) (Seq.map snd edges |> Set.ofSeq)

    { zeros = zeros; counts = counts }

let isFinished (state: State): bool = state.zeros.IsEmpty

[<EntryPoint>]
let main args =
    let edges =
        System.IO.File.ReadAllLines("day07.txt")
        |> Array.map (fun s -> s.[5], s.[36])

    let adj = Array.fold (fun (acc: Map<char, Set<char>>) (x, y) ->
                          if acc.ContainsKey x
                          then acc.Add(x, acc.[x].Add(y))
                          else acc.Add(x, Set.singleton y)) Map.empty edges

    let initState = initStateFromEdges edges

    printfn "%A" (headNode initState)

    0
