let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

[<EntryPoint>]
let main args =
    let input =
        System.IO.File.ReadAllLines("day06.txt")
        |> Array.toList
        |> List.map (fun s -> s.Split(", "))
        |> List.map (function [|x; y|] -> int x, int y | _ -> failwith "WTF")

    let minx, maxx =
        List.map fst input
        |> (fun xs -> List.min xs, List.max xs)
    let miny, maxy =
        List.map snd input
        |> (fun ys -> List.min ys, List.max ys)

    let n = List.length input

    let closest p =
        let dists = List.map (distance p) input
        let minDist = List.min dists
        let ps = List.zip [1 .. n] dists
                 |> List.filter (fun xx -> snd xx = minDist)
        if List.length ps <> 1
          then None
          else List.head ps |> fst |> Some          

    let idx = [for x in minx .. maxx do
               for y in miny .. maxy do
               yield x, y]

    let topElems = [for x in minx .. maxx do yield closest (x, miny)]
    let bottomElems = [for x in minx .. maxx do yield closest (x, maxy)]
    let leftElems = [for y in miny .. maxy do yield closest (minx, y)]
    let rightElems = [for y in miny .. maxy do yield closest (maxx, y)]
    let borderElems = List.concat [topElems; bottomElems; leftElems; rightElems]
                      |> List.choose id
                      |> Set.ofList

    List.countBy closest idx
    |> List.choose (function (Some idx, x) -> Some (idx, x) | _ -> None)
    |> List.filter (fun vv -> not (Set.contains (fst vv) borderElems))
    |> List.map snd
    |> List.max
    |> printfn "%d"

    0

