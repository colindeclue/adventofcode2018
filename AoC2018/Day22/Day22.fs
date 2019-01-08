namespace AoC2018
open System
open System.Collections.Generic

module Day22 =
    // 0 is none, 1 is climbing, 2 is tortch
    type InRegion = {point:int*int;region:int;mutable tool:int}

    // 0 is none, 1 is climbing, 2 is torch
    [<CustomComparison; StructuralEquality>]
    type Node = {cost:int;point:InRegion;distance:int;parent:Option<Node>}
                    interface IComparable<Node> with
                                member this.CompareTo other =
                                    if this.cost <> other.cost then
                                        compare this.cost other.cost
                                    else
                                        compare this.point other.point
                    interface IComparable with
                        member this.CompareTo(obj: obj) =
                            match obj with
                            | :? Node -> compare this (unbox<Node> obj)
                            | _ -> invalidArg "obj" "Must be of type Node"

    let memoize2D ni nj invalid (f : (int -> int -> 'a) -> int -> int -> 'a) =
      let mem = Array2D.create ni nj invalid
      let rec g i j =
        h g i j
      and h r i j =
        if 0 <= i && i < ni && 0 <= j && j < nj then
          match mem.[i, j] with
          | value when value <> invalid -> value
          | value ->
            let value = f g i j
            mem.[i, j] <- value
            value
        else
          f g i j
      g

    // let goal = (10,10)
    // let depth = 510

    let goal = (5,746)
    let depth = 4002

    let buildGrid (depth:int) (maxX:int) (maxY:int) (extra:int) =
        let asList = new List<(int*int)*int>()
        let grid = Array.init (maxY + extra + 1) (fun i -> (Array.create (maxX + extra + 1) -1))
        for y = 0 to maxY + extra do
            for x = 0 to maxX + extra do
                if x = 0 && y = 0 then
                    grid.[y].[x] <- 0
                elif x = maxX && y = maxY then
                    grid.[y].[x] <- 0
                elif x = 0 then
                    grid.[y].[x] <- ((y * 48271) + depth) % 20183
                elif y = 0 then
                    grid.[y].[x] <- ((x * 16807) + depth) % 20183
                else
                    let left = grid.[y].[x - 1]
                    let right = grid.[y - 1].[x]
                    grid.[y].[x] <- ((left * right) + depth) % 20183                
                asList.Add((x,y),grid.[y].[x])
        grid,asList

    let getErosionIndex r (x:int) (y:int) =
        if (x,y) = goal then
            0
        elif x= 0 && y = 0 then
            0
        elif x = 0 then
            ((y * 48271) + depth) % 20183
        elif y = 0 then
            ((x * 16807) + depth) % 20183
        else
            (((r (x - 1) y) * (r x (y - 1))) + depth) % 20183

    let memoed = memoize2D (500) (500) -1 getErosionIndex
    

    let buildSeq (x:int) (y:int) = 
        seq{
            for x' = 0 to x do
                for y' = 0 to y do
                    yield (x',y')
        }

    let printRegion (yMax:int) (xMax:int) (regions:Map<(int*int),int>) =
        let grid = Array.init (yMax + 1) (fun i -> (Array.create (xMax + 1) '#'))
        let mutable sum = 0
        for y = 0 to yMax do
            for x = 0 to xMax do
            let region = Map.find (x,y) regions
            sum <- sum + region
            match region with
            | 2 ->
                grid.[y].[x] <- '|'
            | 1 ->
                grid.[y].[x] <- '='
            | 0 ->
                grid.[y].[x] <- '.'
            | _ -> failwith ("Didn't expect this! " + (string region))
            

        for line in grid do
            line |> String |> printfn "%A"
        sum

    let part1 (depth:int) (endX:int) (endY:int) =
        let regions = buildSeq endX endY |> Seq.map (fun p -> p,(((memoed (fst p) (snd p))) % 3))
        regions |> Seq.map snd |> Seq.sum |> printfn "%A"
        printRegion endY endX

    let getNeighbors (point:int*int) =
        let x = fst point
        let y = snd point
        let mutable points = [(x,y+1);(x + 1, y)]
        if x > 0 then
            points <- (x-1,y)::points
        if y > 0 then
            points <- (x,y-1)::points
        
        points

    let allowedTools = dict[
        0,[|1;2|];
        1,[|0;1|];
        2,[|0;2|]
    ]

    let getNeighborSpaces (current:InRegion) (regions:Map<(int*int),int>) =
        let mutable out = List.empty<InRegion>
        let points = getNeighbors current.point
        for point in points do
            let newPoint = point
            if regions.ContainsKey newPoint then
                let newRegion = Map.find newPoint regions
                // out <- {point=newPoint;region=newRegion;tool=current.tool}::out
                let newTools = allowedTools.[newRegion]
                if newTools.[0] = current.tool || newTools.[1] = current.tool then
                    out <- {point=newPoint;region=newRegion;tool=current.tool}::out
                else
                    let possibleTools = Set.intersect (Set.ofSeq allowedTools.[current.region]) (Set.ofSeq newTools)
                    for tool in possibleTools do // I think just one
                        out <- {point=newPoint;region=newRegion;tool=tool}::out
        out

    let distance (point1:int*int) (point2:int*int) =
        (abs ((fst point1) - (fst point2))) + (abs ((snd point1) - (snd point2)))

    let rec findGoal (regions:Map<(int*int),int>) (current:InRegion) (goal:int*int) (path:InRegion list) =
        if current.point = goal then
            [path]
        else
            let mutable out = List.empty<InRegion list>
            for neighbor in getNeighborSpaces current regions do
                if not (List.contains neighbor path) then
                    out <- List.append (findGoal regions neighbor goal path) out
            out

    let pathScore (path:InRegion list) =
        let moves = path.Length
        let changes = Seq.windowed 2 path |> Seq.filter (fun rs -> rs.[0].tool <> rs.[1].tool) |> Seq.length
        moves + (changes * 7)

    let getPathBasic (regions:Map<(int*int),int>) (start:int*int) (goal:int*int) =
        let startRegion = {point=start;region=0;tool=2}
        let asNode = {cost=0;point=startRegion;distance=0;parent=None}
        let openList = new SortedSet<Node>([|asNode|])
        let mutable closedList = [(startRegion,asNode)] |> Map.ofList
        let mutable openListMap = [(startRegion,asNode)] |> Map.ofList
        let mutable keepGoing = true
        let mutable foundGoal = startRegion
        while keepGoing do
            let current = openList.Min
            openList.Remove current |> ignore
            if current.point.point = goal then
                keepGoing <- false
                foundGoal <- current.point
            else
                for next in getNeighborSpaces current.point regions do
                    let mutable newCost = current.distance + 1
                    if next.tool <> current.point.tool then
                        newCost <- newCost + 7
                    if next.point = goal && next.tool <> 2 then
                        next.tool <- 2
                        newCost <- newCost + 7
                    let priority = (distance next.point goal) + newCost
                    let newNode = {cost=priority;point=next;distance=newCost;parent=Some current}
                    let mutable inOpen = false
                    let mutable inClose = false
                    if Map.containsKey next openListMap then
                        let opened = Map.find next openListMap
                        inOpen <- opened.distance <= newNode.distance
                    if Map.containsKey next closedList then
                        let closed = Map.find next closedList
                        inClose <- closed.distance <= newNode.distance
                    if not (inOpen || inClose) then
                        openList.Add(newNode) |> ignore
            closedList <- closedList.Add(current.point, current)

        let mutable maxX = 0;
        let mutable maxY = 0;
        let mutable path = List.empty<InRegion>
        if closedList.ContainsKey foundGoal then
            let mutable currentNode = Map.find foundGoal closedList
            while currentNode.point <> startRegion do
                let next = currentNode.parent
                if (fst next.Value.point.point) > maxX then
                    maxX <- (fst next.Value.point.point)
                if (snd next.Value.point.point) > maxY then
                    maxY <- (snd next.Value.point.point)
                path <- next.Value.point::path
                currentNode <- next.Value
        path.Length |> printfn "%A"
        // path |> printfn "%A"
        (maxX, maxY) |> printfn "%A"
        foundGoal, (Map.find foundGoal closedList).distance

    let part2 (depth:int) (maxX:int) (maxY:int) =
        let grid,list = buildGrid depth maxX maxY 100
        let regionMap = list |> Seq.map (fun g -> (fst g), (snd g) % 3) |> Map.ofSeq
        regionMap |> printfn "%A"
        getPathBasic regionMap (0,0) (maxX,maxY)

    let part1New (depth:int) (maxX:int) (maxY:int) =
        let grid,list = buildGrid depth maxX maxY 0
        let regionMap = list |> Seq.map (fun g -> (fst g), (snd g) % 3) |> Map.ofSeq
        printRegion maxX maxY regionMap
        // grid |> Array.concat |> Array.map (fun e -> e % 3) |> Array.sum
