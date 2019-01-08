namespace AoC2018
open System
open System.Collections.Generic

module Day22 =
    // 0 is none, 1 is climbing, 2 is tortch
    type InRegion = {point:int*int;region:int;tool:int}

    // 0 is none, 1 is climbing, 2 is torch
    [<CustomComparison; StructuralEquality>]
    type Node = {cost:int;point:InRegion}
                    interface IComparable<Node> with
                                member this.CompareTo other =
                                    if this.cost <> other.cost then
                                        compare this.cost other.cost
                                    else
                                        compare this.point.tool other.point.tool
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

    let printRegion (yMax:int) (xMax:int) =
        let grid = Array.init (yMax + 1) (fun i -> (Array.create (xMax + 1) '#'))
        let mutable sum = 0
        for y = 0 to yMax do
            for x = 0 to xMax do
            let region = memoed x y % 3
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
        let mutable points = [(x + 1, y);(x,y+1)]
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
                let newTools = allowedTools.[newRegion]
                if newTools.[0] = current.tool || newTools.[1] = current.tool then
                    out <- {point=newPoint;region=newRegion;tool=current.tool}::out
                else
                    let possibleTools = Set.intersect (Set.ofSeq allowedTools.[current.region]) (Set.ofSeq newTools)
                    for tool in possibleTools do // I think just one
                        out <- {point=newPoint;region=newRegion;tool=tool}::out
        out |> printfn "%A"
        out

    let distance (point1:int*int) (point2:int*int) =
        (abs (fst point1) - (fst point2)) + (abs (snd point2) - (snd point2))

    let getPath (regions:Map<(int*int),int>) (start:int*int) (goal:int*int) =
        let startRegion = {point=start;region=0;tool=2}
        let asNode = {cost=0;point=startRegion}
        let frontier = new SortedSet<Node>([|asNode|])
        let mutable cameFrom = [(startRegion,startRegion)] |> Map.ofList
        let mutable costSoFar = [(startRegion,0)] |> Map.ofList
        let mutable regionMap = [(start,startRegion)] |> Map.ofList
        let mutable keepGoing = true
        let mutable foundGoal = startRegion
        while keepGoing do
            if frontier.Count = 0 then
                keepGoing <- false
            else            
                let current = frontier.Min
                frontier.Remove current |> ignore
                if current.point.point = goal then
                    foundGoal <- current.point
                    keepGoing <- false
                else
                    for next in getNeighborSpaces current.point regions do
                        let previousCost =
                            match costSoFar.TryFind current.point with
                            | Some (c) -> c
                            | None -> 0
                        let mutable newCost = previousCost + 1
                        if next.tool <> current.point.tool then
                            newCost <- newCost + 7
                        // if next.tool <> 2 && next.point = goal then
                        //     newCost <- newCost + 7
                        let theCostSoFar =
                            match costSoFar.TryFind next with
                            | Some (c) -> c
                            | None -> Int32.MaxValue
                        if newCost < theCostSoFar then
                            costSoFar <- costSoFar.Add (next,newCost)
                            let priority =  newCost + (distance next.point goal)
                            let asNode = {cost=priority;point=next}
                            frontier.Add(asNode) |> ignore
                            cameFrom <- cameFrom.Add (next,current.point)
                            regionMap <- regionMap.Add (next.point,next)

        let mutable current = foundGoal
        let mutable path = List.empty<InRegion>
        if cameFrom.ContainsKey foundGoal then
            while current <> startRegion do
                let next = (Map.find current cameFrom)
                path <- next::path
                current <- next
        path.Length |> printfn "%A"
        path |> printfn "%A"
        Map.find foundGoal costSoFar, Map.find goal regionMap

    let part2 (depth:int) (endX:int) (endY:int) =
        let regions = buildSeq (endX + 40) (endY + 40) |> Seq.map (fun p -> p,(((memoed (fst p) (snd p))) % 3))
        let regionMap = regions |> Map.ofSeq
        getPath regionMap (0,0) (endX,endY)