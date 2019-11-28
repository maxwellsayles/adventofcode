// fsharpc DisjointSet.fs day25.fs -o day25.exe

open DisjointSet

let input : int [][] =
    System.IO.File.ReadAllLines("day25.txt")
    |> Array.map (fun (s: string) -> s.Split([|','|]))
    |> Array.map (Array.map int)

let distance (p1: int []) (p2: int []) : int =
    Array.map2 (fun a b -> abs (a - b)) p1 p2
    |> Array.sum

[<EntryPoint>]
let main args =
    let initSet = DisjointSet.ofList (List.ofSeq input)
    let n = Array.length input
    [ for i in [0 .. n - 2] do
      for j in [i + 1 .. n - 1] do
      let a = input.[i]
      let b = input.[j]
      if distance a b <= 3
      then yield DisjointSet.makeEquivalent a b ]
    |> List.fold (fun ds f -> f ds) initSet
    |> DisjointSet.compressAll
    |> DisjointSet.countEquivalenceClasses
    |> printfn "%d"

    0
