type State = {
    heads: Set<char>;
    inDegree: Map<char, int>;
}

let headNode (state: State): char = state.heads.MinimumElement

let tailState (adj: Map<char, Set<char>>) (state: State): State =
    let hd = headNode state
    let edgeNodes = adj.[hd]

    let inDegree' =
        Set.fold (fun (acc: Map<char, int>) y -> acc.Add(y, acc.[y] - 1))
                 state.inDegree
                 edgeNodes

    let heads' =
        Set.union (state.heads.Remove hd)
                  (Set.filter (fun n -> inDegree'.[n] = 0) edgeNodes)

    { heads = heads'; inDegree = inDegree' }

let isFinished (state: State): bool = state.heads.IsEmpty

let statesString (adj: Map<char, Set<char>>) (state: State): string =
    let rec helper (state: State): seq<char> =
        if isFinished state
        then Seq.empty
        else seq { yield headNode state; yield! helper (tailState adj state) }
    helper state |> System.String.Concat

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

    let initState = {
        heads = Set.difference outNodes inNodes;
        inDegree = Seq.map snd edges |> Seq.countBy id |> Map.ofSeq;
    }

    printfn "%s" (statesString adj initState)

    0
