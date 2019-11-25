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
type Id = Team * int

type State = {
    attackPoints: int
    attackType: AttackType
    hitPoints: int
    id: Id
    immunities: Set<AttackType>
    initiative: int
    units: int
    weaknesses: Set<AttackType>
} with
    member this.team : Team = fst this.id
    member this.group : int = snd this.id

    member this.effectivePower =
        this.units * this.attackPoints

    member this.damageTo (that: State) : int =
        if Set.contains this.attackType that.immunities
        then 0
        elif Set.contains this.attackType that.weaknesses
        then 2 * this.effectivePower
        else this.effectivePower

    member this.attack (that: State) : State =
        let dmg = this.damageTo that
        let unitsLost = dmg / that.hitPoints |> min that.units
        let units' = that.units - unitsLost
        printfn "%A group %d attacks %A group %d, killing %d units" this.team this.group that.team that.group unitsLost
        { that with units = units' }

    member this.isAlive : bool =
        this.units > 0

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

let parseLine (id: Id) (s: string) : State =
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
          units = int units
          weaknesses = weaknesses
        }
    | _ -> failwith <| sprintf "Could not parse: %s" s

let initStates : list<State> =
    let lines = System.IO.File.ReadAllLines("day24.txt")
    let immune =
        Seq.takeWhile (fun (s: string) -> s.Length <> 0) lines
        |> List.ofSeq
        |> List.tail
        |> List.mapi (fun i -> parseLine (ImmuneSystem, i + 1))
    let infect =
        Seq.skip (Seq.length immune + 2) lines
        |> List.ofSeq
        |> List.tail
        |> List.mapi (fun i -> parseLine (Infection, i + 1))
    List.append immune infect

let selectionPhase (states: list<State>) : list<Id * option<Id>> =
    let states' =
        List.sortBy selectionSortKey states
        |> List.rev

    let targetHelper (s: State) ts =
        let ts' = List.filter (fun (t: State) -> t.team <> s.team) ts
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
            let ts' = List.filter (fun u -> u.id <> t.id) ts
            (s.id, Some t.id) :: acc, ts'

    List.fold step (List.empty, states) states
    |> fst
    |> List.rev

let attackPhase (states: list<State>) (selections: list<Id * option<Id>>) : list<State> =
    let helper (acc: Map<Id, State>) (sid, otid) =
        let s = Map.find sid acc
        if s.isAlive then
            match otid with
            | None -> acc
            | Some tid ->
                let t = Map.find tid acc
                let t' = s.attack t
                Map.add tid t' acc
        else
            acc

    let initAcc =
        List.map (fun s -> s.id, s) states
        |> Map.ofList

    selections
    |> List.sortBy (fun (sid, _) -> Map.find sid initAcc |> fun s -> s.initiative)
    |> List.rev
    |> List.fold helper initAcc
    |> Map.toList
    |> List.map snd
    |> List.filter (fun s -> s.isAlive)

let stepGame (states: List<State>) : list<State> =
    selectionPhase states
    |> attackPhase states

let isGameOver (states: List<State>) : bool =
    Seq.groupBy (fun (s: State) -> s.team) states
    |> Seq.length
    |> fun n -> n = 1

let solve1 : int =
    let rec helper states =
        if isGameOver states then
            printfn "%A" states
            List.map (fun s -> s.units) states
            |> List.sum
        else
            helper (stepGame states)
    helper initStates

[<EntryPoint>]
let main args =
    printfn "%d" solve1
    0
