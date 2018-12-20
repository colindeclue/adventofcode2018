namespace AoC2018
open System.IO
open System

module Day18 =
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

    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let size = 50

    let parseGrid (lines:seq<string>) =
        let mutable y = 0
        let mutable x = 0
        let grid = Array.init size (fun i -> (Array.create size '.'))
        for line in lines do
            x <- 0
            for c in line do
                grid.[y].[x] <- c
                x <- x + 1
            y <- y + 1
        grid

    let printGrid (grid:char array[]) =
        for line in grid do
            line |> String |> printfn "%A"

    let safeGetValue (grid:char array[]) (point:int*int) =
        let x = fst point
        let y = snd point
        if x < 0 || x >= size || y < 0 || y >= size then
            'x'
        else
            grid.[y].[x]

    let getNeighbors (point:int*int) =
        let x = fst point
        let y = snd point
        [|(x-1,y);(x-1,y-1);(x,y-1);(x+1,y-1);(x+1,y);(x+1,y+1);(x,y+1);(x-1,y+1)|]

    let safeGetCount (goal:char) (counts:(char*int)[]) =
        let itemMaybe = counts |> Array.tryFindIndex (fun c -> (fst c) = goal)
        match itemMaybe with
        | Some i -> (snd counts.[i])
        | None -> 0

    let getNextValue (grid:char array[]) (point:int*int) =
        let current = grid.[snd point].[fst point]
        let neighborCounts = point |> getNeighbors |> Array.map (safeGetValue grid) |> Array.countBy (fun c -> c)
        let treeCount = neighborCounts |> safeGetCount '|'
        let freeCount = neighborCounts |> safeGetCount '.'
        let yardCount = neighborCounts |> safeGetCount '#'
        match current with
        | '.' -> if treeCount >= 3 then '|' else '.'
        | '|' -> if yardCount >= 3 then '#' else '|'
        | '#' -> if yardCount >= 1 && treeCount >=1 then '#' else '.'
        | _ -> 'x'

    let processGrid (grid:char array[]) =
        let grid' = Array.init size (fun i -> (Array.create size '.'))
        for y = 0 to size - 1 do
            for x = 0 to size - 1 do
                grid'.[y].[x] <- getNextValue grid (x,y)

        grid'

    let part1 input minutes =
        let mutable grid = readLines input |> parseGrid
        printGrid grid
        let memo = memoize processGrid
        for i = 1 to minutes do
            grid <- memo grid
            if i % 1000 = 0 then
                grid |> Array.concat |> Array.countBy (fun c -> c) |> Array.sort |> printfn "%A"
                i |> printfn "%A"
        printGrid grid
        grid |> Array.concat |> Array.countBy (fun c -> c)