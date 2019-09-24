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

let ipReg, instrs : int * V<Instr> =
    let lines =
        System.IO.File.ReadAllLines("day19.txt")
        |> List.ofSeq
    let ipReg =
        List.head lines
        |> fun (s: String) -> s.Split [|' '|]
        |> fun (xs: String []) -> int xs.[1]
    let instrs =
        List.tail lines
        |> List.map parseInstruction
        |> V.ofSeq
    ipReg, instrs

let rec run (rs: Regs) : Regs =
    let ip = V.nth ipReg rs
    match V.tryNth ip instrs with
    | Some instr -> 
        let rs' = exec instr rs
        let ip' = V.nth ipReg rs'
        run <| V.update ipReg (ip' + 1) rs'
    | None -> rs

let rec getArg (rs: Regs) : int =
    let ip = V.nth ipReg rs
    if ip = 1
    then V.nth 4 rs
    else
        match V.tryNth ip instrs with
        | Some instr -> 
            let rs' = exec instr rs
            let ip' = V.nth ipReg rs'
            getArg <| V.update ipReg (ip' + 1) rs'
        | None -> failwith "WTF!"

let sumDivisors (n: int) : int =
    [1..n]
    |> List.filter (fun i -> n % i = 0)
    |> List.sum

[<EntryPoint>]
let main args = 
    printfn "#ip: %d" ipReg

    let part1 : int =
        let rs = V.ofSeq [0; 0; 0; 0; 0; 0]
        let rs' = run rs
        V.nth 0 rs'
    printfn "Part 1: %d" part1

    let part1' : int =
        V.ofSeq [0; 0; 0; 0; 0; 0]
        |> getArg
        |> sumDivisors
    printfn "Part 1': %d" part1'

    let part2 : int =
        V.ofSeq [1; 0; 0; 0; 0; 0]
        |> getArg
        |> sumDivisors
    printfn "Part 2: %d" part2
        
    0
