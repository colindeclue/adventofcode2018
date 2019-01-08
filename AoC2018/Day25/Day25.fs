namespace AoC2018
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

module Day25 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    type Vector = {x:int;y:int;z:int;t:int}

    let parseVector (line:string) =
        match line with
        | Regex @"(-?\d+),(-?\d+),(-?\d+),(-?\d+)" [x;y;z;t] -> {x=int x;y=int y;z=int z;t=int t}
        | _ -> {x=0;y=0;z=0;t=0}

    let distance (v1:Vector) (v2:Vector) =
        (abs (v2.x - v1.x)) +
        (abs (v2.y - v1.y)) +
        (abs (v2.z - v1.z)) +
        (abs (v2.t - v1.t))

    let canJoin (constellation:List<Vector>) (v:Vector) =
        constellation |> Seq.map (fun v2 -> distance v v2) |> Seq.min <= 3

    let areOne (constellation:List<Vector>) (constellation2:List<Vector>) =
        constellation |> Seq.exists (fun v -> canJoin constellation2 v)

    let reduceConstellationList (constellations:List<List<Vector>>) =
        let outList = new List<List<Vector>>()
        let alreadyCombined = new List<List<Vector>>()
        for i = 0 to constellations.Count - 1 do
            let constellation = constellations.[i]
            if not (Seq.contains constellation alreadyCombined) then
                let rest = constellations |> Seq.skip (i + 1)
                for item in rest do
                    if not (Seq.contains item alreadyCombined) then
                        if areOne constellation item then
                            constellation.AddRange(item)
                            alreadyCombined.Add(item)
                outList.Add(constellation)

        outList

    let part1 input =
        let pointsSeq = readLines input |> Seq.map parseVector |> Seq.map (fun v -> new List<Vector>([v]))
        let mutable points = new List<List<Vector>>(pointsSeq)
        let mutable moreFound = true
        let allConstellations = new List<List<Vector>>()
        while moreFound do
            let previousCount = points.Count
            points <- reduceConstellationList points
            let newCount = points.Count
            if previousCount = newCount then
                moreFound <- false

        points.Count

