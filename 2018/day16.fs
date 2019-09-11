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

type Example = {
    before: Regs;
    after: Regs;
    opCodeIdx: int;
    args: int [];
    }

let getValues (s: String) : int [] =
    s 
    |> String.filter (fun (c: char) -> Char.IsNumber(c) || c = ' ')
    |> fun (s: String) -> s.Split [|' '|]
    |> Array.filter (fun (s: String) -> s <> "")
    |> Array.map (fun (s: String) -> int(s))

let parseExample (chunk: list<String>) : Example =
    let before = getValues chunk.[0]
    let instr = getValues chunk.[1]
    let after = getValues chunk.[2]

    {
        before = V.ofSeq before;
        after = V.ofSeq after;
        opCodeIdx = instr.[0];
        args = [| instr.[1]; instr.[2]; instr.[3] |];
    }

let parseExamples (lines: list<String>) : list<Example> =
    let rec helper xs acc =
        if List.isEmpty xs
        then List.rev acc
        else helper (List.skip 4 xs) ((parseExample (List.take 3 xs)) :: acc)
    helper lines List.empty

let filterValidOps (ops: list<OpCode>) (ex: Example) : list<OpCode> =
    let validOp (op: OpCode) =
        let instr = Instr (op, ex.args.[0], ex.args.[1], ex.args.[2])
        exec instr ex.before = ex.after
    List.filter validOp ops

let examples : list<Example> =
    System.IO.File.ReadAllLines("day16-1.txt")
    |> List.ofSeq
    |> parseExamples

let part1 : int =
    examples
    |> List.map (fun (ex: Example) -> filterValidOps allOpCodes ex)
    |> List.filter (fun xs -> List.length xs >= 3)
    |> List.length

[<EntryPoint>]
let main args = 
    printfn "%d" part1
    0
