// fsharpc -r:FSharpx.Collections.dll day19.fs

open FSharpx.Collections
open System

module V = PersistentVector
type V<'a> = PersistentVector<'a>

type Regs = V<int>

type Instr = Instr of String * int * int * int

let exec (Instr (op, a, b, c)) (rs: Regs) : Regs =
    match op with
    | "addr" -> V.update c (V.nth a rs + V.nth b rs) rs
    | "addi" -> V.update c (V.nth a rs + b) rs
    | "mulr" -> V.update c (V.nth a rs * V.nth b rs) rs
    | "muli" -> V.update c (V.nth a rs * b) rs
    | "banr" -> V.update c (V.nth a rs &&& V.nth b rs) rs
    | "bani" -> V.update c (V.nth a rs &&& b) rs
    | "borr" -> V.update c (V.nth a rs ||| V.nth b rs) rs
    | "bori" -> V.update c (V.nth a rs ||| b) rs
    | "setr" -> V.update c (V.nth a rs) rs
    | "seti" -> V.update c a rs
    | "gtir" -> V.update c (if a > V.nth b rs then 1 else 0) rs
    | "gtri" -> V.update c (if V.nth a rs > b then 1 else 0) rs
    | "gtrr" -> V.update c (if V.nth a rs > V.nth b rs then 1 else 0) rs
    | "eqir" -> V.update c (if a = V.nth b rs then 1 else 0) rs
    | "eqri" -> V.update c (if V.nth a rs = b then 1 else 0) rs
    | "eqrr" -> V.update c (if V.nth a rs = V.nth b rs then 1 else 0) rs
    | s -> failwith (sprintf "WTF: %s" s)

let parseInstruction (s: String) : Instr =
    let xs = s.Split [|' '|]
    Instr (xs.[0], int xs.[1], int xs.[2], int xs.[3])

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

let rec getArg (rs: Regs) : int =
    let ip = V.nth ipReg rs
    if ip = 28
    then V.nth 3 rs
    else
        let instr = instrs.[ip]
        let rs' = exec instr rs
        let ip' = V.nth ipReg rs'
        getArg <| V.update ipReg (ip' + 1) rs'

let mutable cnt = 0
let rec getRep (rs: Regs) (acc: Set<int>) : int =
    let ip = V.nth ipReg rs
    let rs' =
        let instr = instrs.[ip]
        let rs' = exec instr rs
        let ip' = V.nth ipReg rs'
        V.update ipReg (ip' + 1) rs'
    if ip = 28 then
        let r3 = V.nth 3 rs
        printfn "%d" cnt
        cnt <- cnt + 1
        if Set.contains r3 acc then
            r3
        else
            getRep rs' (Set.add r3 acc)
    else
        getRep rs' acc

[<EntryPoint>]
let main args = 
    printfn "#ip: %d" ipReg
    let r3 = getArg (V.ofSeq [0; 0; 0; 0; 0; 0])
    printfn "ip=28, r3=%d" r3

    let r3' = getRep (V.ofSeq [0; 0; 0; 0; 0; 0]) Set.empty
    printfn "ip=28, r3=%d" r3'
        
    0
