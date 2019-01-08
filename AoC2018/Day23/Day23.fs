namespace AoC2018
open System
open System.IO
open System.Text.RegularExpressions


module Day23 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let getX (input) =
        match input with
        | (x,y,z) -> x

    let getY (input) =
        match input with
        | (x,y,z) -> y

    let getZ (input) =
        match input with
        | (x,y,z) -> z

    type NanoBot = {x:int64;y:int64;z:int64;r:int64}

    let botLivesInBox (bot:NanoBot) (lower:int64*int64*int64) (upper:int64*int64*int64) =
        (bot.x >= getX lower && bot.x <= getX upper) &&
        (bot.y >= getY lower && bot.y <= getY upper) &&
        (bot.z >= getZ lower && bot.z <= getZ upper)

    let botIntersectsCube (bot:NanoBot) (lower:int64*int64*int64) (upper:int64*int64*int64) =
        let mutable distSquared = bot.r
        if bot.x < getX lower then
            distSquared <- distSquared - (abs (bot.x - (getX lower)))
        elif bot.x > getX upper then
            distSquared <- distSquared - (abs (bot.x - (getX upper)))
        if bot.y < getY lower then
            distSquared <- distSquared - (abs (bot.y - (getY lower)))
        elif bot.y > getY upper then
            distSquared <- distSquared - (abs (bot.y - (getY upper)))
        if bot.z < getZ lower then
            distSquared <- distSquared - (abs (bot.z - (getZ lower)))
        elif bot.z > getZ upper then
            distSquared <- distSquared - (abs (bot.z - (getZ upper)))

        distSquared >= 0L

    
    let inRange (lower:int64*int64*int64) (upper:int64*int64*int64) (bot:NanoBot) =
        (botIntersectsCube bot lower upper) || (botLivesInBox bot lower upper)

    let distance (first:NanoBot) (second:NanoBot) =
        (abs (first.x - second.x)) + (abs (first.y - second.y)) + (abs (first.z - second.z))

    let parseBot input =
        match input with
        | Regex @"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" [x;y;z;r] -> {x=int64 x;y=int64 y;z=int64 z;r=int64 r}
        | _ -> failwith ("Not a good bot " + input)

    let part1 input =
        let bots = input |> readLines |> Seq.map parseBot
        let maxBot = bots |> Seq.maxBy (fun b -> b.r)
        let inRange = bots |> Seq.map (distance maxBot) |> Seq.filter (fun d -> d <= maxBot.r) |> Seq.length
        (maxBot, inRange)

    let point64sIn (lower:int64*int64*int64) (upper:int64*int64*int64) =
        lower |> printfn "%A"
        upper |> printfn "%A"
        (abs ((getX upper) - (getX lower))) *
        (abs ((getY upper) - (getY lower))) *
        (abs ((getZ upper) - (getZ lower)))

    let mostPopulatedEigth (bots:NanoBot list) (lower:int64*int64*int64) (upper:int64*int64*int64) =
        // Scale is the max radius. We maximize for the number of bots with the smallest radius
        let midX = ((getX lower) + (getX upper)) / 2L
        let midY = ((getY lower) + (getY upper)) / 2L
        let midZ = ((getZ lower) + (getZ upper)) / 2L
        let q1 = lower,(midX, midY, midZ)
        let q2 = (getX lower, midY, getZ lower),(midX, getY upper, midZ)
        let q3 = (midX, getY lower, getZ lower),(getX upper, midY, midZ)
        let q4 = (midX, midY, getZ lower),(getX upper, getY upper, midZ)
        let q5 = (getX lower, getY lower, midZ),(midX, midY, getZ upper)
        let q6 = (getX lower, midY, midZ),(midX, getY upper, getZ upper)
        let q7 = (midX, getY lower, midZ),(getX upper, midY, getZ upper)
        let q8 = (midX, midY, midZ),upper
        let q1Test = inRange (fst q1) (snd q1)
        let q2Test = inRange (fst q2) (snd q2)
        let q3Test = inRange (fst q3) (snd q3)
        let q4Test = inRange (fst q4) (snd q4)
        let q5Test = inRange (fst q5) (snd q5)
        let q6Test = inRange (fst q6) (snd q6)
        let q7Test = inRange (fst q7) (snd q7)
        let q8Test = inRange (fst q8) (snd q8)
        let q1Scored = (q1, bots |> Seq.filter q1Test |> Seq.length)
        let q2Scored = (q2, bots |> Seq.filter q2Test |> Seq.length)
        let q3Scored = (q3, bots |> Seq.filter q3Test |> Seq.length)
        let q4Scored = (q4, bots |> Seq.filter q4Test |> Seq.length)
        let q5Scored = (q5, bots |> Seq.filter q5Test |> Seq.length)
        let q6Scored = (q6, bots |> Seq.filter q6Test |> Seq.length)
        let q7Scored = (q7, bots |> Seq.filter q7Test |> Seq.length)
        let q8Scored = (q8, bots |> Seq.filter q8Test |> Seq.length)
        let scores = [|q1Scored;q2Scored;q3Scored;q4Scored;q5Scored;q6Scored;q7Scored;q8Scored|]
        scores |> Seq.maxBy snd

    let botsInRange (bots:NanoBot list) (point64:int64*int64*int64) =
        let asBot = {x=getX point64;y=getY point64;z=getZ point64;r=0L}
        bots |> Seq.filter (fun b -> (distance asBot b) <= b.r) |> Seq.length

    let getPoint64s (lower:int64*int64*int64) (upper:int64*int64*int64) =
        seq {
            for x = getX lower to getX upper do
                for y = getY lower to getY upper do
                    for z = getZ lower to getZ upper do
                        yield (x,y,z)
        }

    let part2 input = 
        let bots = input |> readLines |> Seq.map parseBot |> Seq.toList
        let maxBot = bots |> Seq.maxBy (fun b -> b.r)
        let mutable min = Int64.MaxValue
        let mutable max = Int64.MinValue
        for bot in bots do
            if bot.x < min then
                min <- bot.x
            if bot.x > max then
                max <- bot.x
            if bot.y < min then
                min <- bot.y
            if bot.y > max then
                max <- bot.y
            if bot.z < min then
                min <- bot.z
            if bot.z > max then
                max <- bot.z
              
        let mutable search = mostPopulatedEigth bots (min,min,min) (max,max,max)
        let mutable pointsIn = point64sIn (fst (fst search)) (snd (fst search))
        while pointsIn < 0L || pointsIn > (10000L) do
            search <- mostPopulatedEigth bots (fst (fst search)) (snd (fst search))
            pointsIn <- point64sIn (fst (fst search)) (snd (fst search))
        let bounds = (fst search)
        let checkPoint64s = getPoint64s (fst bounds) (snd bounds)
        let bestPoint64 = checkPoint64s |> Seq.maxBy (botsInRange bots)
        bestPoint64 |> printfn "%A"
        botsInRange bots bestPoint64 |> printfn "%A"
        (abs (getX bestPoint64)) + (abs (getY bestPoint64)) + (abs (getZ bestPoint64))