open System.Text.RegularExpressions

type GuardShift = { id: int; naps: list<int * int>; }

let isBeginsShift str = Regex.Match(str, "Guard").Success
let guardID str = Regex.Match(str, "Guard #(\d+)").Groups.[1].Value |> int
let minute str = Regex.Match(str, "\d+:(\d+)").Groups.[1].Value |> int

let ranges : seq<string> -> list<int * int> =
    List.ofSeq
    >> List.map minute
    >> List.chunkBySize 2
    >> List.map (function [x; y] -> x, y | _ -> failwith "WTF")

let rec groupInput input =
  if Seq.isEmpty input then Seq.empty
  else let id = Seq.head input |> guardID
       let naps = Seq.takeWhile (not << isBeginsShift) (Seq.tail input) |> ranges
       let group = { id = id; naps = naps }
       let n = 1 + 2 * List.length group.naps
       seq { yield group
             yield! groupInput (Seq.skip n input) }

let guardToNaps (shifts: list<GuardShift>): Map<int, list<int * int>> =
    let rec helper (acc: Map<int, list<int * int>>) (shift: GuardShift) =
        match acc.TryFind shift.id with
        | Some naps -> acc.Add(shift.id, List.append naps shift.naps)
        | None -> acc.Add(shift.id, shift.naps)
    List.fold helper Map.empty shifts

let sumNaps = List.map (fun (x, y) -> y - x) >> List.sum

let minuteWithMostNaps naps =
    let helper (acc: Map<int, int>) (nap: int * int) =
        List.fold (fun (acc: Map<int, int>) (d: int) ->
                   match acc.TryFind d with
                   | Some x -> acc.Add(d, x + 1)
                   | None -> acc.Add(d, 1)
                   ) acc [fst nap .. snd nap - 1]
    List.fold helper Map.empty naps
    |> Map.toSeq
    |> Seq.maxBy snd
    |> fst

[<EntryPoint>]
let main args =
    let input =
        System.IO.File.ReadLines("day04.txt")
        |> List.ofSeq
        |> List.sort

    let naps = input |> groupInput |> List.ofSeq |> guardToNaps
    let guardWithMostNaps = Map.toSeq naps |> Seq.maxBy (snd >> sumNaps)
    let minute = minuteWithMostNaps (snd guardWithMostNaps)
    fst guardWithMostNaps * minute |> printfn "%d"

    0
