
type UnionFind<'T when 'T : comparison> = {
    ids : Map<'T, int>
    parents : Map<int, int>
    } with

    static member empty : UnionFind<'T> =
        { ids = Map.empty; parents = Map.empty }

    member this.add (x: 'T) : UnionFind<'T> =
        let newId = 1 + Seq.length this.ids
        { ids = Map.add x newId this.ids;
          parents = Map.ofList [newId, newId]
          }

    member private this.findRoot (x: 'T) : int * int =
        let rec loop i n =
            let i' = Map.find i this.parents
            if i' = i then i, n else loop i' (n + 1)
        loop (Map.find x this.ids) 0

    member private this.compressPath (x: 'T) (root: int) : UnionFind<'T> =
        let parents' =
            let rec loop acc i =
                let i' = Map.find i this.parents
                let acc' = Map.add i root acc
                if i' = i then acc' else loop acc' i'
            loop this.parents (Map.find x this.ids)
        let ids' = Map.add x root this.ids
        { ids = ids'; parents = parents' }

    member this.union (x: 'T) (y: 'T) : UnionFind<'T> =
        let xr, xn = this.findRoot x
        let yr, yn = this.findRoot y
        let r = if xn > yn then xr else yr
        (this.compressPath x r).compressPath y r

    // Inefficient equivalence test.
    member this.isEquivalentINEFFICIENT (x: 'T) (y: 'T) : bool =
        fst (this.findRoot x) = fst (this.findRoot y)

let input : int [][] =
    System.IO.File.ReadAllLines("day25.txt")
    |> Array.map (fun (s: string) -> s.Split([|','|]))
    |> Array.map (Array.map int)

[<EntryPoint>]
let main args =
    printfn "%A" input

    0
