type NodeState = {
    inDegree: int;
    availableTime: int;
}

type State = {
    heads: seq<char * int>;
    nodes: Map<char, NodeState>;
    workers: List<int>;
}

let processInputEdge (time: int) (node: NodeState): NodeState =
    {
        inDegree = node.inDegree - 1;
        availableTime = max time node.availableTime;
    }

let nextState (adj: Map<char, Set<char>>) (state: State): State =
    // Get the next processing time. This is the max of the earliest available
    // worker and the earliest available node to be processed.
    let workerTime = List.head state.workers
    let nodeTime = Seq.map snd state.heads |> Seq.min
    let time' = max workerTime nodeTime

    // Select all head nodes that are available earlier than the next processing
    // time. These are nodes that are ready for processing.
    let hd =
        Seq.filter (fun (_, t) -> t <= time') state.heads
        |> Seq.map fst
        |> Seq.min

    printfn "Starting %c at %d" hd time'

    let edgeNodes = adj.[hd]
    let processingTime = 61 + int hd - int 'A'

    // The next time this worker will be available. Also the time when the new
    // head nodes can start being processed.
    let availableTime = time' + processingTime

    // Update the worker available time.
    let workers' = availableTime :: List.tail state.workers |> List.sort

    // Reduce the in-degree of all the edge nodes and update their available
    // time.
    let nodes' =
        Set.fold (fun (acc: Map<char, NodeState>) y ->
                  acc.Add(y, processInputEdge availableTime acc.[y]))
                 state.nodes
                 edgeNodes

    // New heads are edge nodes that now have a degree of 0. They are available
    // the next time this worker is available.
    let newHeads =
        Set.filter (fun n -> nodes'.[n].inDegree = 0) edgeNodes
        |> Seq.map (fun n -> n, nodes'.[n].availableTime)

    let heads' = Seq.filter (fun (n, _) -> n <> hd) state.heads
                 |> Seq.append newHeads

    {
        heads = heads';
        nodes = nodes';
        workers = workers';
    }

let isFinished (state: State): bool =
    Seq.isEmpty state.heads

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
        heads = Set.difference outNodes inNodes |> Seq.map (fun n -> n, 0);
        nodes = Seq.map snd edges
                |> Seq.countBy id
                |> Seq.map (fun (n, c) -> n, { inDegree = c; availableTime = 0 })
                |> Map.ofSeq;
        workers = List.replicate 5 0;
    }

    let finalState = solve adj initState
    printfn "%d" (List.max finalState.workers)

    0
