namespace AoC2018
open System.IO
open System.Collections
open System.Collections.Generic

module Day6 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let ringAround (input:int*int) (size:int) =
        let x = fst input
        let y = snd input
        let topBottom = [x - size .. x + size] |> Seq.collect (fun x -> [(x, y - size);(x, y + size)])
        let leftRight = [y - size .. y + size] |> Seq.collect (fun y -> [(x - size, y);(x + size, y)])
        Seq.concat [topBottom;leftRight] |> Seq.distinct

    let infiniteRingAround (input:int*int) =
        let mutable size = 1
        let ringer = ringAround input
        seq { 
            while size < 100 do
                yield ringer size
                size <- size + 1
        }
   
    let middle (input) =
        match input with
        | (a, b, c) -> b

    let last (input) =
        match input with
        | (a, b, c) -> c

    let first (input) =
        match input with
        | (a,b,c) -> a

    let rest (input) =
        match input with
        | (a, b, c) -> (b,c)

    let x1 extremes =
        match extremes with
        | (a, b, c, d) -> a

    let x2 extremes =
        match extremes with
        | (a, b, c, d) -> c
    
    let y1 extremes =
        match extremes with
        | (a, b, c, d) -> b
    
    let y2 extremes =
        match extremes with
        | (a, b, c, d) -> d

    let foldExtremes (extremes:int*int*int*int) (point:char*(int*int)) =
        let second = snd point
        (min (x1 extremes) (fst second), min (y1 extremes) (snd second), max (x2 extremes) (fst second), max (y2 extremes) (snd second))

    let getExtremes (list:seq<char*(int*int)>) =
        Seq.fold foldExtremes (1000,1000,0,0) list

    let getAll (extremes:int*int*int*int) =
        seq {
            for x in [(x1 extremes) .. (x2 extremes)] do
                for y in [(y1 extremes) .. (y2 extremes)] do
                    yield (x,y)
        }

    let getEdges (extremes:int*int*int*int) =
        let topBottom = [x1 extremes .. x2 extremes] |> Seq.collect (fun x -> [(x, y1 extremes);(x, y2 extremes)])
        let leftRight = [y1 extremes .. y2 extremes] |> Seq.collect (fun y -> [(x1 extremes, y);(x2 extremes, y)])
        Seq.concat [topBottom;leftRight] |> Seq.distinct

    let distance (first:int*int) (secondWithName:char*(int*int)) =
        let second = snd secondWithName
        abs (fst first - fst second) + abs(snd first - snd second)

    let toString input =
        match input with
        | (a,b) -> String.concat "," [a |> string;b |> string]

    let memoize f =
        let cache = ref Map.empty
        fun x ->
        match (!cache).TryFind(x) with
            | Some res -> 
                res
            | None ->
                let res = f x
                cache := (!cache).Add(x,res)
                res

    let closest (list:seq<char*(int*int)>) (point:int*int) =
        let distanceFrom = distance point
        let sorted = list |> Seq.map (fun x -> (distanceFrom x,x)) |> Seq.sortBy fst
        if fst (Seq.head sorted) <> (Seq.skip 1 sorted |> Seq.head |> fst) then
            sorted |> Seq.head |> snd |> fst
        else
            '.'

    let allWithin (list:seq<char*(int*int)>) (goal:int) (point:int*int) =
        let distanceFrom = distance point
        (list |> Seq.sumBy distanceFrom) < goal

    let convertPoint (input:string) =
        let array = input.Split [|','|] |> Seq.map int |> Seq.toArray
        (array.[0], array.[1])

    let takeRing (goal:char) (ring:seq<char>) =
        ring |> Seq.exists (fun x -> x = goal)

    let getArea (all:seq<char*(int*int)>) (candidate:(char*(int*int))) =
        let measurer = memoize (closest all)
        let goal = fst candidate
        let rings = infiniteRingAround (snd candidate)
        let taker = takeRing goal
        let area = rings |> Seq.map (fun ring -> ring |> Seq.map measurer) |> Seq.takeWhile taker
        area |> Seq.sumBy (fun x -> x |> Seq.filter (fun y -> y = goal) |> Seq.length)

    let countItem (seq) (x) =  seq |> Seq.filter ((=) x) |> Seq.length

    let part1(input) =
        let points = readLines input |> Seq.map convertPoint
        let names = Seq.append ['a'..'z'] ['A'..'Z']
        let withNames = Seq.zip names points
        let extremes = getExtremes withNames
        let edges = getEdges extremes
        let measurer = memoize (closest withNames)
        let bad = edges |> Seq.map measurer |> Set.ofSeq
        let candidates = withNames |> Seq.filter (fun x -> (not) (bad.Contains(fst x)))
        let allPoints = getAll extremes
        let measured = allPoints |> Seq.map measurer
        let counter = countItem measured
        candidates |> Seq.map (fun x -> (counter (fst x)), (fst x)) |> Seq.maxBy fst

    let part2(input) =
        let points = readLines input |> Seq.map convertPoint
        let goal = 10000
        let names = Seq.append ['a'..'z'] ['A'..'Z']
        let withNames = Seq.zip names points
        let extremes = getExtremes withNames
        let allPoints = getAll extremes
        let within = allWithin withNames goal
        allPoints |> Seq.filter within |> Seq.length

        