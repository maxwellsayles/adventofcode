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
let intExpr (i: int) : ArithExpr = ctx.MkIntConst(string i) :> ArithExpr
let zeroExpr : ArithExpr = intExpr 0
let oneExpr : ArithExpr = intExpr 1

let absExpr (x: ArithExpr) : ArithExpr =
    ctx.MkITE(ctx.MkLt(x, zeroExpr), ctx.MkSub(zeroExpr, x), x)
    :?> ArithExpr

let distanceExpr (x1: ArithExpr) (x2: ArithExpr) : ArithExpr =
    ctx.MkSub(x2, x1)
    |> absExpr

let distance3Expr ((x1, y1, z1): ArithExpr * ArithExpr * ArithExpr) ((x2, y2, z2): ArithExpr * ArithExpr * ArithExpr) : ArithExpr =
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
        List.map (fun (bot: NanobotState) -> inRangeExpr p (botExpr bot) (intExpr bot.r)) input
    let sum = List.fold (fun acc expr -> ctx.MkAdd(acc, expr)) zeroExpr inRangeExprs
    let minDist = distance3Expr p (zeroExpr, zeroExpr, zeroExpr)
    let opt = ctx.MkOptimize()
    opt.MkMaximize(sum) |> ignore
    opt.MkMinimize(minDist) |> ignore
    opt.Check() |> ignore
    printfn "%A" (opt.Model.ConstInterp(x))
    printfn "%A" (opt.Model.ConstInterp(y))
    printfn "%A" (opt.Model.ConstInterp(z))


    // let ctx = new Context()
    // let x = ctx.MkIntConst("x")
    // let y = ctx.MkIntConst("y")
    // let z = ctx.MkIntConst("z")
    // let c1 = ctx.MkGe(x, ctx.MkInt("1"))
    // let c2 = ctx.MkEq(z, ctx.MkInt("5"))
    // let c3 = ctx.MkLt(x, y)
    // let c4 = ctx.MkLt(y, z)
    // let opt = ctx.MkOptimize()

    // opt.Assert(c1)
    // opt.Assert(c2)
    // opt.Assert(c3)
    // opt.Assert(c4)
    // opt.MkMinimize(x) |> ignore
    // opt.MkMaximize(y) |> ignore
    // opt.Check() |> ignore
    // printfn "%A" (opt.Model.ConstInterp(x))
    // printfn "%A" (opt.Model.ConstInterp(y))
    // printfn "%A" (opt.Model.ConstInterp(z))

    0

