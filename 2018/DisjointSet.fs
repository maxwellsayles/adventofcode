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

let union (x: 'T) (y: 'T) (ds: DisjointSet<'T>): DisjointSet<'T> =
    let xr = findRoot x ds
    compressPath x xr ds |> compressPath y xr


