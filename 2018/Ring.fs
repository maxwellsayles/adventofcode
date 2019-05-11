module Ring

type Ring = {
    xs : list<int>;
    ys : list<int>;
}

let balance ring =
    let n = List.length ring.xs
    let m = List.length ring.ys
    if n >= 2 * m || m >= 2 * n then
        let vs = List.append ring.xs (List.rev ring.ys)
        let p = (n + m) / 2
        { xs = List.take p vs;
          ys = List.skip p vs |> List.rev }
    else ring

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
