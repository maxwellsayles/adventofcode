let input = 6042

let powerLevel x y =
    let rackID = x + 10
    let v = (rackID * y + input) * rackID
    ((v / 100) % 10) - 5

let grid =
    Array2D.init 300 300 (fun x y -> powerLevel (x + 1) (y + 1))

let sum3x3 x y =
    [ for i in [0..2] do
      for j in [0..2] do
      yield grid.[x + i, y + j]
    ]
    |> List.sum

let gridSum =
    Array2D.init 298 298 sum3x3

let rollingSum =
    let roll (acc: Map<int * int, int>) (x, y) =
        let sum =
            match x, y with
            | -1, _ -> 0
            | _, -1 -> 0
            | 0, 0 -> grid.[0, 0]
            | x, 0 -> acc.[x - 1, 0] + grid.[x, 0]
            | 0, y -> acc.[0, y - 1] + grid.[0, y]
            | x, y ->
                acc.[x - 1, y] +
                acc.[x, y - 1] -
                acc.[x - 1, y - 1] +
                grid.[x, y]
        Map.add (x, y) sum acc

    let coords = [ for x in [-1..299] do
                   for y in [-1..299] do
                   yield x, y ]

    List.fold roll Map.empty coords

let triples =
  [ for x in [0..299] do
    for y in [0..299] do
    let m = min (299 - x) (299 - y)
    for s in [1..m] do
    yield x, y, s ]

let area x y s =
    rollingSum.[x + s - 1, y + s - 1] -
    rollingSum.[x - 1, y + s - 1] -
    rollingSum.[x + s - 1, y - 1] +
    rollingSum.[x - 1, y - 1]

[<EntryPoint>]
let main args =
    // Part 1
    [| for x in [1..298] do
       for y in [1..298] do
       yield (x, y), gridSum.[x - 1, y - 1]
    |]
    |> Array.maxBy snd
    |> fst
    |> printfn "%A"

    // Part 2
    List.map (fun (x, y, s) -> (x, y, s), area x y s) triples
    |> List.maxBy snd
    |> fst
    |> fun (x, y, s) -> x + 1, y + 1, s
    |> printfn "%A"

    0
