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

type Id = int
type Team = ImmuneSystem | Infection

type State = {
    attackPoints: int
    attackType: AttackType
    hitPoints: int
    id: Id
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

let parseLine (id: int) (team: Team) (s: string) : State =
    match s with
    | Regex @"(\d+) units each with (\d+) hit points (\(.*\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)" [units; hitPoints; extra; attackPoints; attackType; initiative] ->
        let weaknesses, immunities =
            parseWeaknessesAndImmunities extra
        { attackPoints = int attackPoints
          attackType = stringToAttack attackType
          hitPoints = int hitPoints
          id = id
          immunities = immunities
          initiative = int initiative
          team = team
          units = int units
          weaknesses = weaknesses
        }
    | _ -> failwith <| sprintf "Could not parse: %s" s

let initStates : list<State> =
    let lines = System.IO.File.ReadAllLines("day24-example.txt")
    let immune =
        Seq.takeWhile (fun (s: string) -> s.Length <> 0) lines
        |> List.ofSeq
        |> List.tail
        |> List.mapi (fun i -> parseLine i ImmuneSystem)
    let n = List.length immune
    let infect =
        Seq.skip (Seq.length immune + 2) lines
        |> List.ofSeq
        |> List.tail
        |> List.mapi (fun i -> parseLine (i + n) Infection)
    List.append immune infect

let selectionPhase (states: list<State>) : list<Id * option<Id>> =
    let states' = List.sortBy selectionSortKey states
    let targetHelper s ts =
        let ts' = List.filter (fun t -> t.team <> s.team) ts
        if List.isEmpty ts' then
            None
        else
            List.maxBy (targetSortKey s) ts'
            |> Some
    let step (acc, ts) s =
        match targetHelper s ts with
        | None ->
            (s.id, None) :: acc, ts
        | Some t ->
            let ts' = List.filter (fun xx -> xx <> t) ts
            (s.id, Some t.id) :: acc, ts'

    List.fold step (List.empty, states) states
    |> fst

[<EntryPoint>]
let main args =
    printfn "%A" initStates
    printfn ""
    printfn "%A" (selectionPhase initStates)

    0
