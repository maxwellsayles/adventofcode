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

[<EntryPoint>]
let main args =
    [| for x in [1..298] do
       for y in [1..298] do
       yield (x, y), gridSum.[x - 1, y - 1]
    |]
    |> Array.maxBy snd
    |> fst
    |> printfn "%A"

    0
