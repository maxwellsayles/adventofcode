(**
 * Build with
 * $ fsharpc -a Ring.fs
 * $ fsharpc --reference:Ring.dll day09.fs
 *
 * or as a single command line:
 * $ fsharpc -a Ring.fs && fsharpc --reference:Ring.dll day09.fs && ./day09.exe
 *)

open Ring

let numPlayers = 464
//let numMarbles = 70918 // Part 1
let numMarbles = 7091800

let step x ring =
    if (x % 23) <> 0
    then
        Ring.spinCCW 2 ring
        |> Ring.push x
        |> fun ring -> 0, ring
    else 
        let ring' = Ring.spinCW 7 ring
        x + Ring.top ring', Ring.pop ring'

let rec gameStep x ring (scores: Map<int, uint64>) =
    if x = numMarbles + 1
    then scores // Game over!
    else 
        let score, ring' = step x ring
        let player = (x % numPlayers) + 1
        let scores' = Map.add player (scores.[player] + uint64 score) scores
        gameStep (x + 1) ring' scores'

[<EntryPoint>]
let main args =
    let initScores =
        List.init numPlayers (fun i -> i + 1, 0UL)
        |> Map.ofList
    let finalScores = gameStep 1 (Ring.singleton 0) initScores
    let winningScore = finalScores |> Map.toList |> List.map snd |> List.max
    printfn "%d" winningScore
    0
