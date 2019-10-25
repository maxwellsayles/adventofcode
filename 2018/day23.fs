open Microsoft.Z3
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

type NanobotState = {
    x: int
    y: int
    z: int
    r: int
}

let tokenizeLine (s: string) : NanobotState =
    match s with
    | Regex @"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" [ x; y; z; r ] ->
        { x = int x; y = int y; z = int z; r = int r }
    | _ -> sprintf "WTF: %s" s |> failwith

let input : list<NanobotState> =
    System.IO.File.ReadAllLines("day23.txt")
    |> Array.map tokenizeLine
    |> List.ofSeq

let dist (ns1: NanobotState) (ns2: NanobotState) : int =
    abs (ns1.x - ns2.x) + abs (ns1.y - ns2.y) + abs (ns1.z - ns2.z)

let part1 : int =
    let largest = List.maxBy (fun ns -> ns.r) input
    let largestR = largest.r
    input
    |> List.filter (fun ns -> dist largest ns <= largestR)
    |> List.length

let ctx = new Context()
let intExpr (i: int) : ArithExpr = ctx.MkInt(string i) :> ArithExpr
let zeroExpr : ArithExpr = intExpr 0
let oneExpr : ArithExpr = intExpr 1

let absExpr (x: ArithExpr) : ArithExpr =
    ctx.MkITE(ctx.MkGt(x, zeroExpr), x, ctx.MkSub(zeroExpr, x))
    :?> ArithExpr

let distanceExpr (x2: ArithExpr) (x1: ArithExpr) : ArithExpr =
    ctx.MkSub(x2, x1)
    |> absExpr

let distance3Expr ((x2, y2, z2): ArithExpr * ArithExpr * ArithExpr)
                  ((x1, y1, z1): ArithExpr * ArithExpr * ArithExpr) : ArithExpr =
    ctx.MkAdd(distanceExpr x2 x1, distanceExpr y2 y1, distanceExpr z2 z1)

let inRangeExpr (p1: ArithExpr * ArithExpr * ArithExpr) (p2: ArithExpr * ArithExpr * ArithExpr) (r: ArithExpr) : ArithExpr =
    ctx.MkITE(ctx.MkLe(distance3Expr p2 p1, r), oneExpr, zeroExpr)
    :?> ArithExpr

let botExpr (bot: NanobotState) : ArithExpr * ArithExpr * ArithExpr =
    intExpr bot.x, intExpr bot.y, intExpr bot.z

[<EntryPoint>]
let main args =
    printfn "%d" part1

    let x = ctx.MkIntConst("x")
    let y = ctx.MkIntConst("y")
    let z = ctx.MkIntConst("z")
    let p = x, y, z

    let inRangeExprs =
        Array.ofSeq input
        |> Array.map (fun (bot: NanobotState) -> inRangeExpr p (botExpr bot) (intExpr bot.r))
    let sum = ctx.MkAdd(inRangeExprs)
    let minDist = distance3Expr p (zeroExpr, zeroExpr, zeroExpr)
    let opt = ctx.MkOptimize()
    let sumHandle = opt.MkMaximize(sum)
    let minDistHandle = opt.MkMinimize(minDist)
    printfn "%A" (opt.Check())
    let xint = sprintf "%A" (opt.Model.ConstInterp(x)) |> int
    let yint = sprintf "%A" (opt.Model.ConstInterp(y)) |> int
    let zint = sprintf "%A" (opt.Model.ConstInterp(z)) |> int
    printfn "%d" (abs xint + abs yint + abs zint)

    0

