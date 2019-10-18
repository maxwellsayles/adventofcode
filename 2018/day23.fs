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

[<EntryPoint>]
let main args =
    printfn "%d" part1
    0

