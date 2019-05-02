type State = {
    heads: Set<char * int>;
    inDegree: Map<char, int>;
    workers: List<int>;
}

let nextState (adj: Map<char, Set<char>>) (state: State): State =
    // Get the next processing time. This is the max of the earliest available
    // worker and the earliest available node to be processed.
    let workerTime = List.head state.workers
    let nodeTime = Set.map snd state.heads |> Set.minElement
    let time' = max workerTime nodeTime

    // Select all head nodes that are available earlier than the next processing
    // time. These are nodes that are ready for processing.
    let hd, hdTime =
        Set.filter (fun (_, t) -> t <= time') state.heads
        |> Set.minElement

    let edgeNodes = adj.[hd]
    let processingTime = 61 + int hd - int 'A'

    // The next time this worker will be available. Also the time when the new
    // head nodes can start being processed.
    let availableTime = time' + processingTime

    // Update the worker available time.
    let workers' = availableTime :: List.tail state.workers |> List.sort

    // Reduce the in-degree of all the edge nodes.
    let inDegree' =
        Set.fold (fun (acc: Map<char, int>) y -> acc.Add(y, acc.[y] - 1))
                 state.inDegree
                 edgeNodes

    // New heads are edge nodes that now have a degree of 0. They are available
    // the next time this worker is available.
    let newHeads =
        Set.filter (fun n -> inDegree'.[n] = 0) edgeNodes
        |> Set.map (fun n -> n, availableTime)

    let heads' = Set.union (state.heads.Remove (hd, hdTime)) newHeads

    {
        heads = heads';
        inDegree = inDegree'
        workers = workers';
    }

let isFinished (state: State): bool =
    state.heads.IsEmpty

let solve (adj: Map<char, Set<char>>) (state: State): State =
    let rec helper (state: State): State =
        if isFinished state
        then state
        else helper (nextState adj state)
    helper state

[<EntryPoint>]
let main args =
    let edges =
        System.IO.File.ReadAllLines("day07.txt")
        |> Array.map (fun s -> s.[5], s.[36])

    let outNodes = Seq.map fst edges |> Set.ofSeq
    let inNodes = Seq.map snd edges |> Set.ofSeq
    let nodes = Set.union outNodes inNodes

    let adj =
        Array.fold (fun (acc: Map<char, Set<char>>) (x, y) -> acc.Add(x, acc.[x].Add(y)))
                   (Seq.map (fun n -> n, Set.empty) nodes |> Map.ofSeq)
                   edges

    let initState = {
        heads = Set.difference outNodes inNodes |> Set.map (fun n -> n, 0);
        inDegree = Seq.map snd edges |> Seq.countBy id |> Map.ofSeq;
        workers = List.replicate 5 0;
    }

    let finalState = solve adj initState
    printfn "%A" finalState.workers
//    printfn "%d" (List.max finalState.workers)

    0
