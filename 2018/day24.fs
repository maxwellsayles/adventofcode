type AttackType =
    | Bludgeoning
    | Cold
    | Fire
    | Radiation
    | Slashing

let stringToAttack : string -> AttackType = function
    | "bludgeoning" -> Bludgeoning
    | "cold" -> Cold
    | "fire" -> Fire
    | "radiation" -> Radiation
    | "slashing" -> Slashing
    | unknown -> failwith <| sprintf "Unrecognized attack: %s" unknown

type State = {
    attackPoints: int
    attackType: AttackType
    hitPoints: int
    immunities: list<AttackType>
    initiative: int
    units: int
    weaknesses: list<AttackType>
}

let parseLine = 0

let rawImmuneSystem, rawInfection =
    let lines = System.IO.File.ReadAllLines("day24.txt")
    let immune =
        Seq.takeWhile (fun (s: string) -> s.Length <> 0) lines
        |> List.ofSeq
        |> List.tail
    let infect =
        Seq.skip (Seq.length immune + 2) lines
        |> List.ofSeq
        |> List.tail
    immune, infect

[<EntryPoint>]
let main args =
    printfn "%A" rawImmuneSystem
    printfn ""
    printfn "%A" rawInfection

    0
