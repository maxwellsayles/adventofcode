(**
 * This is a specialized implementation of a disjoint set. In particular this
 * is intended for a four phase use case. The first use case populates the
 * set members, the second phase creates the equivalance classes, the third
 * phase compresses the data structure, and the fourth phase queries the number
 * of equivalent classes. This is not meant to be general purpose or efficient.
 *)

module DisjointSet

type DisjointSet<'T when 'T : comparison> = {
    ids : Map<'T, int>
    parents : Map<int, int>
    }

let empty : DisjointSet<'T> =
    { ids = Map.empty; parents = Map.empty }

let add (x: 'T) (ds: DisjointSet<'T>) : DisjointSet<'T> =
    let newId = Seq.length ds.ids
    {
        ids = Map.add x newId ds.ids
        parents = Map.add newId newId ds.parents
    }

let ofList (xs: list<'T>) : DisjointSet<'T> =
    {
        ids = List.mapi (fun i x -> x, i) xs |> Map.ofList
        parents = List.mapi (fun i _ -> i, i) xs |> Map.ofList
    }

let private findRoot (x: 'T) (ds: DisjointSet<'T>) : int =
    let rec loop i =
        let i' = Map.find i ds.parents
        if i' = i then i else loop i'
    loop (Map.find x ds.ids)

let private compressPath (x: 'T) (root: int) (ds: DisjointSet<'T>) : DisjointSet<'T> =
    let parents' =
        let rec loop acc i =
            let i' = Map.find i ds.parents
            let acc' = Map.add i root acc
            if i' = i then acc' else loop acc' i'
        loop ds.parents (Map.find x ds.ids)
    let ids' = Map.add x root ds.ids
    { ids = ids'; parents = parents' }

let makeEquivalent (x: 'T) (y: 'T) (ds: DisjointSet<'T>) : DisjointSet<'T> =
    let xr = findRoot x ds
    compressPath x xr ds |> compressPath y xr

let compressAll (ds: DisjointSet<'T>) : DisjointSet<'T> =
    let helper ds x = compressPath x (findRoot x ds) ds
    List.fold helper ds (Map.toList ds.ids |> List.map fst)

let countEquivalenceClasses (ds: DisjointSet<'T>) : int =
    Map.toList ds.parents
    |> List.filter (fun (i, j) -> i = j)
    |> List.length
