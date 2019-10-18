open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

type NanobotState = {
    x: int
    y: int
    z: int
    r: int
}

let tokenizeLine (s: string) : NanobotState =
    match s with
    | Regex @"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" [ x; y; z; r ] ->
        { x = int x; y = int y; z = int z; r = int r }
    | _ -> sprintf "WTF: %s" s |> failwith

let input : list<NanobotState> =
    System.IO.File.ReadAllLines("day23.txt")
    |> Array.map tokenizeLine
    |> List.ofSeq

let dist (ns1: NanobotState) (ns2: NanobotState) : int =
    abs (ns1.x - ns2.x) + abs (ns1.y - ns2.y) + abs (ns1.z - ns2.z)

let part1 : int =
    let largest = List.maxBy (fun ns -> ns.r) input
    let largestR = largest.r
    input
    |> List.filter (fun ns -> dist largest ns <= largestR)
    |> List.length

let countInRange ((x, y, z): int * int * int) (bots: list<NanobotState>) : int =
    let ns0 = { x = x; y = y; z = z; r = 0 }
    Seq.filter (fun ns -> dist ns0 ns <= ns.r) bots
    |> Seq.length

let bounds (ns: NanobotState) : list<int * int * int> =
    [ ns.x - ns.r, ns.y, ns.z
      ns.x + ns.r, ns.y, ns.z
      ns.x, ns.y - ns.r, ns.z
      ns.x, ns.y + ns.r, ns.z
      ns.x, ns.y, ns.z - ns.r
      ns.x, ns.y, ns.z + ns.r ]
      
let part2 : int =
    List.map bounds input
    |> List.fold List.append []
    |> List.map (fun p -> p, countInRange p input)
    |> Seq.maxBy (fun ((x, y, z), c) -> c, -(abs x + abs y + abs z))
    |> fun ((x, y, z), _) -> abs x + abs y + abs z

[<EntryPoint>]
let main args =
    printfn "%d" part1
    printfn "%d" part2
    0

