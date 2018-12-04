namespace AoC2018
open System.Text.RegularExpressions
open System.IO

module Day3 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let GetId (input) =
        match input with
        | Regex @"#(\d+)" [id] ->
            id
        | _ -> " "

    let GetOffset (input) =
        match input with
        | Regex @"(\d+),(\d+)" [x;y] -> 
            (x |> int, y |> int)
        | _ -> (0,0)

    let GetSize (input) = 
        match input with
        | Regex @"(\d+)x(\d+)" [x;y] ->
            (x |> int, y|> int)
        | _ -> (0,0)

    let xMax (point) (size) =
        fst point + fst size - 1
    
    let yMax (point) (size) =
        snd point + snd size - 1

    let LeftOf (point) (size) (other) (otherSize) =
        (fst point) > (xMax other) otherSize ||
        (fst other) > (xMax point) size
    
    let Above (point) (size) (other) (otherSize) =
        (snd point) > (yMax other) otherSize ||
        (snd other) > (yMax point) size

    let Collides (point,size) (other,otherSize) = 
        not ((point,size) = (other,otherSize)) &&
        not (LeftOf point size other otherSize) &&
        not (Above point size other otherSize)

    let anyCollide (rects) (rect) =
        rects |> Seq.exists (Collides rect)
    let maxXs (point) (size) (other) (otherSize) =
        seq [xMax point size; xMax other otherSize]
     
    let xStarts (point) (size) (other) (otherSize) =
        seq [fst point; fst other]

    let maxYs (point) (size) (other) (otherSize) =
        seq [yMax point size; yMax other otherSize]

    let yStarts (point) (size) (other) (otherSize) =
        seq [snd point; snd other]

    let GetRects (input) =
        (GetId input, GetOffset input, GetSize input)
    
    let GetRectsAndPoints (input) =
        let rect = (GetId input, GetOffset input, GetSize input)
        (GetId input, GetOffset input, GetSize input, enumerate (tail rect))

    let countItem (seq) (x) =  seq |> Seq.filter ((=) x) |> Seq.length

    let enumerate (point,size) =
        seq {
            for x = (fst point) to (fst point) + (fst size) - 1 do
                for y = (snd point) to (snd point) + (snd size) - 1 do
                    yield (x,y)
        }

    let countCollisions (seq) =
        let mutable uniques = Set.empty
        let mutable result = Set.empty
        for p in seq do
            if uniques.Contains(p) then
                result <- result.Add(p)
            else
                uniques <- uniques.Add(p)
        result.Count

    let exceptWithDuplicates (first) (second) =
        first |> Seq.filter (fun x -> not (Seq.contains x second))
        
    let uniqueRec (rects) (allPoints) =
        let allPointsLength = allPoints |> Seq.length
        let mutable answer = ""
        let except = exceptWithDuplicates allPoints
        for rect in rects do
            let ourPoints = (last rect)
            let without = except ourPoints
            let withoutCount = without |> Seq.length
            let ourCount = ourPoints |> Seq.length
            if allPointsLength - ourCount = withoutCount then
                answer <- (head rect)
        
        answer

    let head (tup) =
        match tup with
        | (a, b, c) -> a

    let tail (tup) =
        match tup with
        | (a, b, c) -> (b, c)
    

    let part1 (input) =
        readLines input |> Seq.map GetRects |> Seq.map tail |> Seq.map enumerate |> Seq.concat |> Seq.toList |> countCollisions
    
    let part2 (input) =
        let rects = readLines input |> Seq.map GetRects
        let allRectsWithoutName = rects |> Seq.map tail
        let mutable answer = ""
        let evaluate = anyCollide allRectsWithoutName
        for rect in rects do
            if not (evaluate (tail rect)) then
                answer <- (head rect)
        answer
