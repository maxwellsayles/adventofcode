// fsharpc -r:FSharpx.Collections.dll day16

open FSharpx.Collections

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

[<EntryPoint>]
let main args = 
  0
