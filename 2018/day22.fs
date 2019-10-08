let depth : int = 3879
let target : int * int = 8, 713
let gridWidth : int = fst target + 1
let gridHeight : int = snd target + 1

let grid : int [,] =
    let res = Array2D.create gridWidth gridHeight 0
    let es = Array2D.create gridWidth gridHeight 0
    let fillTerrain (x: int) (y: int) =
        let geoIdx =
            if x = 0 && y = 0 then
                0
            elif x = fst target && y = snd target then
                0
            elif x = 0 then
                y * 48271
            elif y = 0 then
                x * 16807
            else
                es.[x, y - 1] * es.[x - 1, y]
        let errosion = (geoIdx + depth) % 20183
        es.[x, y] <- errosion
        res.[x, y] <- errosion % 3
    for y in [0..snd target] do
        for x in [0..fst target] do
            fillTerrain x y
    res

let part1 : int =
    let mutable sum = 0
    Array2D.iter (fun v -> sum <- sum + v) grid
    sum

[<EntryPoint>]
let main args =
    printfn "%d" part1
    0
