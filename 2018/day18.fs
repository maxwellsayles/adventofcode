let input : char [,] =
    let lines = System.IO.File.ReadAllLines("day18.txt")
    Array2D.init 50 50 (fun x y -> lines.[y].[x])

let lookup (x: int) (y: int) (grid: char [,]) : char =
    if x < 0 || x >= 50 || y < 0 || y >= 50
    then '.'
    else grid.[x, y]

let neighborCoords : (int * int) [] =
    [| -1, -1
     ; -1,  0
     ; -1,  1
     ;  0,  1
     ;  1,  1
     ;  1,  0
     ;  1, -1
     ;  0, -1
     |]

let step (x: int) (y: int) (grid: char [,]) : char =
    let neighbors =
        Array.map (fun (dx, dy) -> lookup (x + dx) (y + dy) grid) neighborCoords
    let count (v: char) = Seq.filter (fun c -> c = v) neighbors |> Seq.length
    match grid.[x, y] with
    | '.' -> if count '|' >= 3 then '|' else '.'
    | '|' -> if count '#' >= 3 then '#' else '|'
    | '#' -> if count '#' >= 1 && count '|' >= 1 then '#' else '.'
    | _ -> failwith "WTF"

let stepGrid (grid: char [,]) : char [,] =
    Array2D.mapi (fun x y _ -> step x y grid) grid

let rec iterate (n: int) (grid: char [,]) : char [,] =
    if n = 0 then grid else stepGrid grid |> iterate (n - 1)

let countGrid (c: char) (grid: char [,]) : int =
    let mutable count = 0
    Array2D.iter (fun v -> if v = c then count <- count + 1 else ()) grid
    count

[<EntryPoint>]
let main (args : string []) : int = 
    let grid10 = iterate 10 input
    printfn "%A" (countGrid '|' grid10 * countGrid '#' grid10)
    0
