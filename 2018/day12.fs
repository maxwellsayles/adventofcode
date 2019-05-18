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

[<EntryPoint>]
let main args = 
    let initState = Seq.toList input
    printfn "%d" solve1
    0
