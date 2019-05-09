type Node = {
    children: list<Node>;
    metadata: list<int>;
}

let rec parseTree (input: list<int>): Node * list<int> =
    let childrenCount = List.head input
    let metadataCount = List.head (List.tail input)
    let children, tail =
        List.fold (fun (nodes, xs) _ ->
                       let node, xs' = parseTree xs
                       (node :: nodes), xs'
                   )
                  (List.empty, List.skip 2 input)
                  [1 .. childrenCount]
    let node = {
        children = List.rev children;
        metadata = List.take metadataCount tail;
    }
    node, (List.skip metadataCount tail)

let rec metadata (node: Node): seq<list<int>> =
    seq {
        yield node.metadata;
        for n in node.children do yield! metadata n;
    }

let rec score(node: Node): int =
    if List.isEmpty node.children
    then List.sum node.metadata
    else let arr = List.map score node.children |> Array.ofList
         List.filter (fun idx -> idx >= 1 && idx <= Array.length arr) node.metadata
         |> List.map (fun idx -> arr.[idx-1])
         |> List.sum

[<EntryPoint>]
let main args =
    let input =
        System.IO.File.ReadAllLines("day08.txt")
        |> Seq.head
        |> fun s -> s.Split [|' '|]
        |> List.ofArray
        |> List.map int

    let root, xs = parseTree input
    if List.isEmpty xs
    then printfn "%d" (metadata root |> List.concat |> List.sum)
    else failwith "Did not consume all input!"

    printfn "%d" (score root)

    0
