// fsharpc -r:FSharpx.Collections.dll day14.fs

open FSharpx.Collections

module PV = PersistentVector

let input = 556061

[<EntryPoint>]
let main args =
    let vec: PersistentVector<int> = PV.ofSeq [3;7]
    printfn "%A" vec
    0
