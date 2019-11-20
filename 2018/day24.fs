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

type Team = ImmuneSystem | Infection

type State = {
    attackPoints: int
    attackType: AttackType
    hitPoints: int
    immunities: Set<AttackType>
    initiative: int
    team: Team
    units: int
    weaknesses: Set<AttackType>
} with
    member this.effectivePower =
        this.units * this.attackPoints

    member this.damageTo (that: State) : int =
        if Set.contains this.attackType that.immunities
        then 0
        elif Set.contains this.attackType that.weaknesses
        then 2 * this.effectivePower
        else this.effectivePower

let selectionSortKey (s: State) =
    s.effectivePower, s.initiative

let targetSortKey (s: State) (t: State) =
    s.damageTo t, t.effectivePower, t.initiative
    
let parseWeaknessesAndImmunities (s: string) : Set<AttackType> * Set<AttackType> =
    let step (ws, is) s =
        match s with
        | Regex @"(immune to |weak to )(.*)" [thing; ls] ->
            let attacks =
                ls.Split [|','|]
                |> Array.map (fun s -> s.Trim(' ', ')'))
                |> Array.map stringToAttack
                |> Set.ofSeq
            if thing = "immune to " then
                (ws, Set.union is attacks)
            else
                (Set.union ws attacks, is)
        | _ -> failwith <| sprintf "Could not parse: %s" s
            
    if s = "" then
        Set.empty, Set.empty
    else
        s.Split [|';'|]
        |> Array.fold step (Set.empty, Set.empty)

let parseLine (team: Team) (s: string) : State =
    match s with
    | Regex @"(\d+) units each with (\d+) hit points (\(.*\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)" [units; hitPoints; extra; attackPoints; attackType; initiative] ->
        let weaknesses, immunities =
            parseWeaknessesAndImmunities extra
        { attackPoints = int attackPoints
          attackType = stringToAttack attackType
          hitPoints = int hitPoints
          immunities = immunities
          initiative = int initiative
          team = team
          units = int units
          weaknesses = weaknesses
        }
    | _ -> failwith <| sprintf "Could not parse: %s" s

let initImmuneSystem, initInfection =
    let lines = System.IO.File.ReadAllLines("day24.txt")
    let immune =
        Seq.takeWhile (fun (s: string) -> s.Length <> 0) lines
        |> List.ofSeq
        |> List.tail
        |> List.map (parseLine ImmuneSystem)
    let infect =
        Seq.skip (Seq.length immune + 2) lines
        |> List.ofSeq
        |> List.tail
        |> List.map (parseLine Infection)
    immune, infect

[<EntryPoint>]
let main args =
    printfn "%A" initImmuneSystem
    printfn ""
    printfn "%A" initInfection

    0
