namespace AoC2018
open System
open System.IO
open System.Text.RegularExpressions

module Day17 =
    type Wall = {x:int;yList:int array}
    type Floor = {xList:int array;y:int}

    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseWall input =
        match input with
        | Regex @"x=(\d+), y=(\d+)\.\.(\d+)" [x;ymin;ymax] -> {x=(int x);yList=[|(int ymin)..(int ymax)|]}
        | _ -> {x= -1;yList=[|-1|]}

    let parseFloor input =
        match input with
        | Regex @"y=(\d+), x=(\d+)\.\.(\d+)" [y;xmin;xmax] -> {y=(int y);xList=[|(int xmin)..(int xmax)|]}
        | _ -> {y= -1;xList=[|-1|]}

    let getFloorsAndWalls (input:seq<string>) =
        let mutable walls = []
        let mutable floors = []
        for line in input do
            if line.[0] = 'x' then
                walls <- List.append walls [(parseWall line)]
            else
                floors <- List.append floors [(parseFloor line)]

        (floors,walls)

    let getMinY (wall:Wall) =
        wall.yList.[0]

    let getMinX (floor:Floor) =
        floor.xList.[0]

    let getMaxY (wall:Wall) =
        Array.last wall.yList

    let getMaxX (floor:Floor) =
        Array.last floor.xList

    let getBounds (walls:seq<Wall>) (floors:seq<Floor>) =
        let wallMinX = walls |> Seq.map (fun w -> w.x) |> Seq.min
        let floorMinX = floors |> Seq.map getMinX |> Seq.min
        let floorMinY = floors |> Seq.map (fun f -> f.y) |> Seq.min
        let wallMinY = walls |> Seq.map getMinY |> Seq.min
        let floorMaxX = floors |> Seq.map getMaxX |> Seq.max
        let wallMaxX = walls |> Seq.map (fun w -> w.x) |> Seq.max
        let floorMaxY = floors |> Seq.map (fun f -> f.y) |> Seq.max
        let wallMaxY = walls |> Seq.map getMaxY |> Seq.max
        (min wallMinX floorMinX, min wallMinY floorMinY),(max wallMaxX floorMaxX, max wallMaxY floorMaxY)

    let buildGrid (bounds:(int*int)*(int*int)) (walls:seq<Wall>) (floors:seq<Floor>) =
        let xOffset = (fst bounds |> fst) - 1
        let yOffset = fst bounds |> snd
        let xCount = (snd bounds |> fst) - xOffset + 3
        let yCount = (snd bounds |> snd) - yOffset + 1
        let grid = Array.init yCount (fun i -> (Array.create xCount '.'))
        for wall in walls do
            for y in wall.yList do
                grid.[y - yOffset].[wall.x - xOffset] <- '#'
            
        for floor in floors do
            for x in floor.xList do
                grid.[floor.y - yOffset].[x - xOffset] <- '#'

        grid,(xOffset,yOffset)

    let printGridPortion (grid:char array[]) (x:int) (y:int) (size:int) =
        let xMin = max 0 (x - size)
        let yMin = max 0 (y - size)
        let xMax = x + size
        let yMax = y + size
        for fullLine in grid.[yMin..yMax] do
            fullLine.[xMin..xMax] |> String |> printfn "%A"

    let printGrid (grid:char array[]) =
        for line in grid do
            line |> String |> printfn "%A"

    let tryFindStop (grid:char array[]) (y:int) (x:int) =
        if y >= grid.Length then
            None
        else 
            let indexMaybe = grid |> Array.skip (y + 1) |> Array.tryFindIndex (fun r -> r.[x] = '#' || r.[x] = '~')
            match indexMaybe with
            | Some i -> Some (i + y)
            | None -> None

    // Finds the left most spill (free space) if it exists, or None otherwise
    let findLeftSpill (grid:char array[]) (y:int) (x:int) =
        let wallBefore = grid.[y].[..x] |> Array.rev |> Array.tryFindIndex (fun c -> c = '#')
        let minX = match wallBefore with
                    | Some i -> x - i
                    | None -> -1
        // We're looking for where it will spill... empty space.
        let endOfWall = grid.[y+1].[..x] |> Array.rev |> Array.tryFindIndex (fun c -> c = '.' || c = '|')
        // If it's none we really shouldn't be in this function idk what to do
        let wallEnd = match endOfWall with
                        | Some i -> x - i
                        | None -> -1
        if minX >= wallEnd then
            None
        else
            Some wallEnd

    // Finds the left most spill (free space) if it exists, or None otherwise
    let findRightSpill (grid:char array[]) (y:int) (x:int) =
        let wallAfter = grid.[y].[x..] |> Array.tryFindIndex (fun c -> c = '#')
        let minX = match wallAfter with
                    | Some i -> i + x
                    | None -> grid.[0].Length - 1
        // We're looking for where this will start to spill. Empty space.
        let endOfWall = grid.[y+1].[x..] |> Array.tryFindIndex (fun c -> c = '.' || c = '|')
        // If it's none we really shouldn't be in this function idk what to do
        let wallEnd = match endOfWall with
                        | Some i -> i + x
                        | None -> 0
        if wallEnd >= minX then
            None
        else
            Some wallEnd

    // Basically detects a bucket -- a floor/water level surrounded by walls
    // Returns None if no spills, seq [(x,y)] of spill(s) if will spill
    let getSpillPoint (grid:char array[]) (y:int) (x:int) (floor:int) =
        let spills = [findLeftSpill grid floor x;findRightSpill grid floor x] |> Seq.filter (fun s -> s.IsSome)
        if Seq.isEmpty spills then
            None
        else
            Some (spills |> Seq.map (fun o -> o.Value))

    // Fills the given row (starting at the given point) with standing water.
    // Assumes you've already determined it won't spill (there's an area surrounded by wall)
    let fillRowStandingWater (grid:char array[]) (y:int) (x:int) =
        let wallBefore = x - (grid.[y].[..x] |> Array.rev |> Array.findIndex (fun c -> c = '#')) + 1
        let wallAfter = x + (grid.[y].[x..] |> Array.findIndex (fun c -> c = '#')) - 1
        for i = wallBefore to wallAfter do
            grid.[y].[i] <- '~'

    let fillSpillToEnd (grid:char array[]) (y:int) (x:int) =
        for i = y to grid.Length - 1 do
            grid.[i].[x] <- '|'

    let fillRowSpill (grid:char array[]) (y:int) (x:int) (startY:int) (spills:int array) =
        // If there's two things in spill, the x min is the first one's x.
        // If there's one, it's either the first one's x (if it's > x) or that's the max and we have to find a wall
        let mutable xMin = -1
        let mutable xMax = -1
        match spills.Length with
        | 2 ->
            xMin <- spills.[0]
            xMax <- spills.[1]
        | 1 ->
            if spills.[0] < x then
                xMin <- spills.[0]
                let xMaxMaybe = (grid.[y].[x..] |> Array.tryFindIndex (fun c -> c = '#'))
                if xMaxMaybe.IsNone then
                    xMax <- grid.[y].Length - 1
                else 
                    xMax <- x + xMaxMaybe.Value - 1
            else
                xMax <- spills.[0]
                let xMinMaybe = (grid.[y].[..x] |> Array.rev |> Array.tryFindIndex (fun c -> c = '#'))
                if xMinMaybe.IsNone then
                    xMin <- 0
                else 
                    xMin <- x - xMinMaybe.Value + 1
        | _ -> failwith "OH OH SPAGHETIOS SPILL FILL"
        for i = xMin to xMax do
            grid.[y].[i] <- '|'
        for j = (max 0 startY) to y do
            grid.[j].[x] <- '|'

    let rec fillUntilSpillWithFloor (grid:char array[]) (y:int) (x:int) (floor:int) =
        let spillsMaybe = getSpillPoint grid y x floor
        match spillsMaybe with
        | Some spills ->
            let spills = spills |> Seq.toArray
            fillRowSpill grid floor x y spills
            match spills.Length with
            | 2 -> [|(spills.[0], floor);(spills.[1], floor)|]
            | _ -> 
                // let x' = if spills.[0] < x then spills.[0] else spills.[0]
                [|(spills.[0],floor)|]
        | None ->
            fillRowStandingWater grid floor x
            let newFloorMaybe = tryFindStop grid y x
            match newFloorMaybe with
            | Some newFloor -> 
                if newFloor >= floor then
                    fillUntilSpillWithFloor grid (y-1) x newFloor
                else
                    fillUntilSpillWithFloor grid y x newFloor
            | None -> failwith "Had a floor, lost a floor. Shouldn't happen."
            

    let fillUntilSpill (grid:char array[]) (y:int) (x:int) =
        let floorMaybe = tryFindStop grid y x
        match floorMaybe with
        | None ->
            fillSpillToEnd grid y x
            [||]
        | Some floor ->
            fillUntilSpillWithFloor grid y x floor

    let printSpillPoints (grid:char array[]) (spillPoints:(int*int)[]) =
        spillPoints |> Array.iter (fun p -> printGridPortion grid (fst p) (snd p) 30)

    let processWater (xOffset:int) (grid:char array[]) =
        let mutable spillPoints = [|(500 - xOffset, 0)|]
        let mutable spilledPoints = [|(500 - xOffset,0)|]
        while spillPoints.Length > 0 do
            spillPoints |> printfn "%A"
            spillPoints <- spillPoints |> Seq.map (fun p -> fillUntilSpill grid (snd p) (fst p)) |> Array.concat |> Array.distinct |> Array.filter (fun s -> not (Array.exists (fun y -> y = s) spilledPoints))
            spilledPoints <- Array.append spilledPoints spillPoints
            printSpillPoints grid spillPoints


    let part1 input =
        let (floors,walls) = readLines input |> getFloorsAndWalls
        let bounds = getBounds walls floors
        let (grid,offset) = buildGrid bounds walls floors
        processWater (fst offset) grid
        grid |> Seq.concat |> Seq.countBy (fun c -> c = '~' || c = '|')

    let part2 input =
        let (floors,walls) = readLines input |> getFloorsAndWalls
        let bounds = getBounds walls floors
        let (grid,offset) = buildGrid bounds walls floors
        processWater (fst offset) grid
        grid |> Seq.concat |> Seq.countBy (fun c -> c = '~')
