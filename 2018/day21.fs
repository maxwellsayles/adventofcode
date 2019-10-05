open System
open System.Collections.Generic

type Regs = int []
type Instr = String * int * int * int

let parseInstruction (s: String) : Instr =
    let xs = s.Split [|' '|]
    (xs.[0], int xs.[1], int xs.[2], int xs.[3])

let ipReg, instrs : int * Instr [] =
    let lines =
        System.IO.File.ReadAllLines("day21.txt")
        |> List.ofSeq
    let ipReg =
        List.head lines
        |> fun (s: String) -> s.Split [|' '|]
        |> fun (xs: String []) -> int xs.[1]
    let instrs =
        List.tail lines
        |> List.map parseInstruction
        |> Array.ofSeq
    ipReg, instrs

let exec (rs: Regs) : unit =
    let ip = rs.[ipReg]
    let (op, a, b, c) = instrs.[ip]
    match op with
    | "addr" -> rs.[c] <- rs.[a] + rs.[b]
    | "addi" -> rs.[c] <- rs.[a] + b
    | "mulr" -> rs.[c] <- rs.[a] * rs.[b]
    | "muli" -> rs.[c] <- rs.[a] * b
    | "banr" -> rs.[c] <- rs.[a] &&& rs.[b]
    | "bani" -> rs.[c] <- rs.[a] &&& b
    | "borr" -> rs.[c] <- rs.[a] ||| rs.[b]
    | "bori" -> rs.[c] <- rs.[a] ||| b
    | "setr" -> rs.[c] <- rs.[a]
    | "seti" -> rs.[c] <- a
    | "gtrr" -> rs.[c] <- if rs.[a] > rs.[b] then 1 else 0
    | "gtir" -> rs.[c] <- if a > rs.[b] then 1 else 0
    | "gtri" -> rs.[c] <- if rs.[a] > b then 1 else 0
    | "eqrr" -> rs.[c] <- if rs.[a] = rs.[b] then 1 else 0
    | "eqir" -> rs.[c] <- if a = rs.[b] then 1 else 0
    | "eqri" -> rs.[c] <- if rs.[a] = b then 1 else 0
    | s -> failwith (sprintf "WTF: %s" s)
    let ip' = rs.[ipReg]
    rs.[ipReg] <- ip' + 1

let rec getArg (rs: Regs) : int =
    let ip = rs.[ipReg]
    if ip = 28 then
        rs.[3]
    else
        exec rs
        getArg rs

let mutable cnt = 0
let rec getRep (rs: Regs) (acc: Set<int>) : int =
    let ip = rs.[ipReg]
    if ip = 28 then
        let r3 = rs.[3]
        printfn "%d" cnt
        cnt <- cnt + 1
        if Set.contains r3 acc then
            r3
        else
            exec rs
            getRep rs (Set.add r3 acc)
    else
        exec rs
        getRep rs acc

[<EntryPoint>]
let main args = 
    printfn "#ip: %d" ipReg
    let r3 = getArg [|0; 0; 0; 0; 0; 0|]
    printfn "ip=28, r3=%d" r3

    let r3' = getRep [|0; 0; 0; 0; 0; 0|] Set.empty
    printfn "ip=28, r3=%d" r3'
        
    0
