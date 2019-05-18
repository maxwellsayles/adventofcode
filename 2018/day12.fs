let input =
    "#....##.#.#.####..#.######..##.#.########..#...##...##...##.#.#...######.###....#...##..#.#....##.##"

let trans = Map.ofList [
    ".#.##", '#';
    ".#.#.", '#';
    "#.#.#", '.';
    ".####", '.';
    ".#...", '.';
    "#..##", '.';
    "..#.#", '#';
    "#.#..", '.';
    "#####", '.';
    "....#", '.';
    "...##", '.';
    "..##.", '.';
    "##.#.", '#';
    "##..#", '.';
    "##...", '#';
    "..###", '#';
    ".##..", '#';
    "###..", '.';
    "#..#.", '.';
    "##.##", '.';
    "..#..", '#';
    ".##.#", '#';
    "####.", '#';
    "#.###", '.';
    "#...#", '#';
    "###.#", '#';
    "...#.", '#';
    ".###.", '.';
    ".#..#", '#';
    ".....", '.';
    "#....", '.';
    "#.##.", '#';
]

let chunk5 xs =
    let rec helper xs n acc =
        if n < 5
        then List.rev acc
        else let x = List.take 5 xs
             helper (List.tail xs) (n - 1) (x :: acc)
    helper xs (List.length xs) List.empty

let step xs =
    chunk5 (List.concat [['.'; '.'; '.']; xs;  ['.'; '.'; '.']])
    |> List.map (fun x -> trans.[System.String.Concat(x)])

let solve1 =
    let initState = Seq.toList input
    let finalState = List.fold (fun xs _ -> step xs) initState [1..20]
    let n = List.length finalState
    List.zip finalState [-20 .. n - 20 - 1]
    |> List.filter (fun (c, _) -> c = '#')
    |> List.map snd
    |> List.sum

// Run this until the delta is constant. Then basic math.
let solve2 () =
    let initState = Seq.toList input
    let rec helper state ii cc =
        let state' = step state
        let n = List.length state'
        let c = List.zip state' [-ii .. n - ii - 1]
                |> List.filter (fun (c, _) -> c = '#')
                |> List.map snd
                |> List.sum
        printfn "%d %d %d" ii (c - cc) c
        helper state' (ii + 1) c
    helper initState 1 0

[<EntryPoint>]
let main args = 
    let initState = Seq.toList input
    printfn "%d" solve1
    // solve2 ()

    // My input repeats at 91, but 100 is nice and round.
    printfn "%A" (2197UL + 15UL * (50000000000UL - 100UL))

    0
