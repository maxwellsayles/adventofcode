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

let grid = Array.init 1000 (fun _ -> Array.create 1000 0)

let incArea (claim: Claim) =
    for y = claim.top to claim.top + claim.height - 1 do
        for x = claim.left to claim.left + claim.width - 1 do
            grid.[y].[x] <- grid.[y].[x] + 1

let noOverlap (claim: Claim) =
    seq { for y = claim.top to claim.top + claim.height - 1 do
          for x = claim.left to claim.left + claim.width - 1 do
          yield grid.[y].[x] }
    |> Seq.forall (fun x -> x = 1)

[<EntryPoint>]
let main args =
    let input =
        System.IO.File.ReadLines("day03.txt")
        |> List.ofSeq
        |> List.map parseLine

    List.iter incArea input
    Array.fold (Array.fold (fun acc x -> if x >= 2 then acc + 1 else acc)) 0 grid
    |> printfn "%d"
    
    List.filter noOverlap input
    |> List.head
    |> (fun (claim: Claim) -> printfn "%d" claim.id)

    0
