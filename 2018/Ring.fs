module Ring

type Ring = {
    xs : list<int>;
    ys : list<int>;
}

let toList ring = List.append ring.xs (List.rev ring.ys)

// Only balance the internal lists if one of them is empty.
let private balance ring =
    if List.isEmpty ring.xs then
        let n = List.length ring.ys / 2
        {
            xs = List.skip n ring.ys |> List.rev;
            ys = List.take n ring.ys;
        }
     elif List.isEmpty ring.ys then
        let n = List.length ring.xs / 2
        {
            xs = List.take n ring.xs;
            ys = List.skip n ring.xs |> List.rev;
        }
    else ring

// This was an alternative strategy for balancing the ring lists to achieve
// amortized O(1) operations, but this is so much slower than the above, 
// because the balancing occurs more frequently.
// let private balance ring =    
//     let n = List.length ring.xs
//     let m = List.length ring.ys
//     if n >= 2 * m || m >= 2 * n then
//         let vs = toList ring
//         let p = (n + m) / 2
//         { xs = List.take p vs;
//           ys = List.skip p vs |> List.rev }
//     else ring

let ofList xs = balance { xs = xs; ys = List.empty }

let singleton x = { xs = [x]; ys = List.empty }

let empty: Ring = { xs = List.empty; ys = List.empty }

let push x ring = balance { xs = x :: ring.xs; ys = ring.ys }

let pop ring =
    match ring.xs, ring.ys with
    | [_], [] -> empty
    | [], [_] -> empty
    | _ -> balance { xs = List.tail ring.xs; ys = ring.ys }
let top ring =
    match ring.xs, ring.ys with
    | [x], [] -> x
    | [], [x] -> x
    | _ -> List.head ring.xs

let rotateCW ring =
    match ring.xs, ring.ys with
    | [_], [] -> ring
    | [], [_] -> ring
    | xs, ys -> balance { xs = List.head ys :: xs; ys = List.tail ys }

let rotateCCW ring =
    match ring.xs, ring.ys with
    | [_], [] -> ring
    | [], [_] -> ring
    | xs, ys -> balance { xs = List.tail xs; ys = List.head xs :: ys }

let spinCW cnt ring = List.fold (fun r _ -> rotateCW r) ring [1..cnt]
let spinCCW cnt ring = List.fold (fun r _ -> rotateCCW r) ring [1..cnt]
