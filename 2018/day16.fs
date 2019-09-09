// fsharpc -r:FSharpx.Collections.dll day16

open FSharpx.Collections

module V = PersistentVector
type V<'a> = PersistentVector<'a>

type Regs = PVI

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
    | _ -> failwith "Unregoznied op"

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
