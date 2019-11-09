open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

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
    | unknown -> failwith <| sprintf "Unrecognized attack: \"%s\"" unknown

type State = {
    attackPoints: int
    attackType: AttackType
    hitPoints: int
    immunities: list<AttackType>
    initiative: int
    units: int
    weaknesses: list<AttackType>
}

let parseWeaknessesAndImmunities (s: string) : list<AttackType> * list<AttackType> =
    let step (ws, is) s =
        match s with
        | Regex @"(immune to |weak to )(.*)" [thing; ls] ->
            let attacks =
                ls.Split [|','|]
                |> Array.map (fun s -> s.Trim(' ', ')'))
                |> Array.map stringToAttack
                |> List.ofSeq
            if thing = "immune to " then
                (ws, List.append is attacks)
            else
                (List.append ws attacks, is)
        | _ -> failwith <| sprintf "Could not parse: %s" s
            
    s.Split [|';'|]
    |> Array.fold step (List.empty, List.empty)

let parseLine (s: string) : State =
    match s with
    | Regex @"(\d+) units each with (\d+) hit points (\(.*\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)" [units; hitPoints; extra; attackPoints; attackType; initiative] ->
        let weaknesses, immunities =
            if extra = "" then
                List.empty, List.empty
            else
                parseWeaknessesAndImmunities extra
        { attackPoints = int attackPoints;
          attackType = stringToAttack attackType;
          hitPoints = int hitPoints;
          immunities = immunities;
          initiative = int initiative;
          units = int units;
          weaknesses = weaknesses;
        }
    | _ -> failwith <| sprintf "Could not parse: %s" s

let initImmuneSystem, initInfection =
    let lines = System.IO.File.ReadAllLines("day24.txt")
    let immune =
        Seq.takeWhile (fun (s: string) -> s.Length <> 0) lines
        |> List.ofSeq
        |> List.tail
        |> List.map parseLine
    let infect =
        Seq.skip (Seq.length immune + 2) lines
        |> List.ofSeq
        |> List.tail
        |> List.map parseLine
    immune, infect

[<EntryPoint>]
let main args =
    printfn "%A" initImmuneSystem
    printfn ""
    printfn "%A" initInfection

    0
