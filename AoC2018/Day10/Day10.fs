namespace AoC2018
open System;
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic

module Day10 =
    type Point = | PointContents of ((int*int)*(int*int))

    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseLine (input) =
        match input with
        | Regex @"position=<[ ]*(-?\d+),[ ]*(-?\d+)> velocity=<[ ]*(-?\d+),[ ]*(-?\d+)>" [x;y;xv;yv] ->
            PointContents ((int x,int y),(int xv,int yv))
        | _ -> PointContents ((0,0),(0,0))

    let getPoint (point:Point) =
        let (PointContents pc) = point
        fst pc

    let getVelocity (point:Point) =
        let (PointContents pc) = point
        snd pc

    let add (first:int*int) (second:int*int) =
        (fst first + fst second, snd first + snd second)

    let movePoint (point:Point) =
        let current = getPoint point
        let velocity = getVelocity point
        PointContents ((add current velocity), velocity)

    
    let printLine (min:int) (max:int) (line:seq<int*int>) =
        // We're assuming at this point these all have the same y. So we now only care about X values.
        let xs = line |> Seq.map fst |> Seq.sort |> Seq.distinct |> Seq.toArray
        let mutable currentIndex = 0
        let mutable printLine = ""
        for i = min to max do
            if currentIndex > xs.Length - 1 then
                printLine <- printLine + "-"
            elif i = xs.[currentIndex] then
                printLine <- printLine + "#"
                currentIndex <- currentIndex + 1                
            elif i < xs.[currentIndex] then
                printLine <- printLine + "-"
            elif i > xs.[currentIndex] then
                printLine <- printLine + "-"
        printLine |> printfn "%A"

    let printBlankLine (min:int) (max:int) = 
        Seq.init (max - min + 1) (fun x -> '-') |> Seq.toArray |> String

    let print (grid:seq<Point>) =
        let points = grid |> Seq.map getPoint 
        let min = points |> Seq.minBy fst |> fst
        let max = points |> Seq.maxBy fst |> fst
        let byRow = points |> Seq.groupBy snd |> Seq.sortBy fst |> Seq.toArray
        let mutable currentIndex = 0
        let top = Array.head byRow |> fst
        let bottom = Array.last byRow |> fst
        let printer = printLine min max
        let blankline = printBlankLine min max
        for i = top to bottom do
            if i = fst byRow.[currentIndex] then
                snd byRow.[currentIndex] |> printer
                currentIndex <- currentIndex + 1                
            elif i < fst byRow.[currentIndex] then
                blankline |> printfn "%A"

    
    let tickAndPrint (grid:seq<Point>) =
        let grid' = grid |> Seq.map movePoint |> Seq.toList
        print grid'
        grid'

    let tick (grid:Point list) =
        grid |> List.map movePoint

    let generateGrids (start:Point list) =
        seq {
            let mutable grid = start
            while true do
                let grid' = tick grid
                grid <- grid'
                yield grid'
        }

    let area (grid:Point list) =
        let points = grid |> List.map getPoint
        let minX = points |> List.minBy fst |> fst
        let maxX = points |> List.maxBy fst |> fst
        let minY = points |> List.minBy snd |> snd
        let maxY = points |> List.maxBy snd |> snd
        ((uint64 maxX) - (uint64 minX)) * ((uint64 maxY) - (uint64 minY))

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

    let part1 (input) =
        let mutable grid = readLines input |> Seq.map parseLine |> Seq.toList
        let mutable keepGoing = true
        let mutable numSecs = 0
        let calc = memoize area
        while keepGoing do
            let grid' = tick grid
            if (calc grid) < (calc grid') then
                keepGoing <- false
            else 
                grid <- grid'
                numSecs <- numSecs + 1
        print grid
        numSecs |> printfn "%A"

    let moreFunctional (input) =
        let grids = readLines input |> Seq.map parseLine |> Seq.toList |> generateGrids
        let calc = memoize area
        let finalGrids = grids |> Seq.windowed 2 |> Seq.takeWhile (fun x -> (calc x.[0]) > (calc x.[1])) |> Seq.map Seq.last |> Seq.toList
        List.last finalGrids |> print
        finalGrids.Length + 1