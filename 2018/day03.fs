open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then [for g in m.Groups -> g.Value] |> List.tail |> Some
    else None

type Claim = {
    id: int;
    left: int;
    top: int;
    width: int;
    height: int;
}

let parseLine str =
    match str with
        | Regex @"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" [ id; left; top; width; height ] ->
            { id = int id;
              left = int left;
              top = int top;
              width = int width;
              height = int height;
              }
        | _ -> failwith "WTF"

[<EntryPoint>]
let main args =
    let input =
        System.IO.File.ReadLines("day03.txt")
        |> List.ofSeq
        |> List.map parseLine
    List.iter (printfn "%A\n") input
    
    0
