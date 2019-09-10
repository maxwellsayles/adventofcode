// fsharpc -r:FSharpx.Collections.dll day16

open FSharpx.Collections
open System

module V = PersistentVector
type V<'a> = PersistentVector<'a>

type Regs = V<int>

type OpCode =
    | Addr | Addi
    | Mulr | Muli
    | Banr | Bani
    | Borr | Bori
    | Setr | Seti
    | Gtir | Gtri | Gtrr
    | Eqir | Eqri | Eqrr

type Instr = Instr of OpCode * int * int * int

let exec (Instr (op, a, b, c)) (rs: Regs) : Regs =
    match op with
    | Addr -> V.update c (V.nth a rs + V.nth b rs) rs
    | Addi -> V.update c (V.nth a rs + b) rs
    | Mulr -> V.update c (V.nth a rs * V.nth b rs) rs
    | Muli -> V.update c (V.nth a rs * b) rs
    | Banr -> V.update c (V.nth a rs &&& V.nth b rs) rs
    | Bani -> V.update c (V.nth a rs &&& b) rs
    | Borr -> V.update c (V.nth a rs ||| V.nth b rs) rs
    | Bori -> V.update c (V.nth a rs ||| b) rs
    | Setr -> V.update c (V.nth a rs) rs
    | Seti -> V.update c a rs
    | Gtir -> V.update c (if a > V.nth b rs then 1 else 0) rs
    | Gtri -> V.update c (if V.nth a rs > b then 1 else 0) rs
    | Gtrr -> V.update c (if V.nth a rs > V.nth b rs then 1 else 0) rs
    | Eqir -> V.update c (if a = V.nth b rs then 1 else 0) rs
    | Eqri -> V.update c (if V.nth a rs = b then 1 else 0) rs
    | Eqrr -> V.update c (if V.nth a rs = V.nth b rs then 1 else 0) rs

let allOpCodes: list<OpCode> = [
    Addr; Addi;
    Mulr; Muli;
    Banr; Bani;
    Borr; Bori;
    Setr; Seti;
    Gtir; Gtri; Gtrr;
    Eqir; Eqri; Eqrr;
    ]

let getValues (s: String) : int [] =
    s 
    |> String.filter (fun (c: char) -> Char.IsNumber(c) || c = ' ')
    |> fun (s: String) -> s.Split [|' '|]
    |> Array.filter (fun (s: String) -> s <> "")
    |> Array.map (fun (s: String) -> int(s))

let part1 : int =
    let inputRaw =
        System.IO.File.ReadAllLines("day16-1.txt")
        |> List.ofSeq
    let rec helper xs acc =
        if List.isEmpty xs
        then acc
        else
            let chunk = List.take 3 xs
            let before = getValues chunk.[0]
            let instr = getValues chunk.[1]
            let after = getValues chunk.[2]

            let rs = V.ofSeq before
            let rs' = V.ofSeq after

            let opsCount =
                allOpCodes
                |> List.map (fun (op: OpCode) -> Instr (op, instr.[1], instr.[2], instr.[3]))
                |> List.map (fun (instr: Instr) -> exec instr rs)
                |> List.filter (fun rs -> rs = rs')
                |> List.length
                
            helper (List.skip 4 xs) (acc + if opsCount >= 3 then 1 else 0) 
    helper inputRaw 0

[<EntryPoint>]
let main args = 
    printfn "%d" part1
    0
