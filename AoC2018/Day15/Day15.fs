namespace AoC2018
open System.IO
open System.Collections.Generic
open System

module Day15 =
    let memoize f =
        let cache = ref Map.empty
        fun x ->
        match (!cache).TryFind(x) with
            | Some res -> 
                "From cache" |> printf "%A"
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

    type Square = {team:char;hp:int;attack:int;x:int;y:int;id:int}

    [<CustomComparison; StructuralEquality>]
    type Node = {cost:float;square:Square}
                    interface IComparable<Node> with
                                member this.CompareTo other =
                                    if this.cost <> other.cost then
                                        compare this.cost other.cost
                                    elif this.square.y <> other.square.y then
                                        compare this.square.y other.square.y
                                    else
                                        compare this.square.x other.square.x
                    interface IComparable with
                        member this.CompareTo(obj: obj) =
                            match obj with
                            | :? Node -> compare this.cost (unbox<Node> obj).cost
                            | _ -> invalidArg "obj" "Must be of type Node"

    let mutable sId = 0;                        
    let getSquare (elfStrength:int) (value:char) (p:int*int) =
        let mutable hp = 0
        let mutable attack = 0
        let mutable id = -1
        if value <> '.' && value <> '#' then
            hp <- 200
            attack <- if value = 'E' then elfStrength else 3
            id <- sId
            sId <- id + 1
        
        {team=value;hp=hp;attack=attack;x=fst p;y=snd p;id=id}

    let parseGrid (elfStrength:int) (lines:seq<string>) =
        let mutable y = 0
        let mutable x = 0
        let mutable grid = Array.empty<array<Square>>
        for line in lines do
            grid <- Array.append grid [|Array.zeroCreate line.Length|]
            for char in line do
                grid.[y].[x] <- getSquare elfStrength char (x,y)
                x <- x + 1
            y <- y + 1
            x <- 0
        grid

    let distance (first:Square) (second:Square) =
        (abs (first.y - second.y)) + (abs (first.x - second.x))

    let getNeighborPoints (s:Square) =
        [|(s.x,s.y-1);(s.x-1,s.y);(s.x+1,s.y);(s.x,s.y+1)|]

    let getNeighborSpaces (s:Square) (grid:Square array[]) =
        getNeighborPoints s |> Seq.map (fun p -> grid.[snd p].[fst p]) |> Seq.filter (fun s -> s.team = '.')

    // Returns the new attackie
    let attack (attacker:Square) (attackie:Square) =
        let newHp = attackie.hp - attacker.attack
        if newHp > 0 then
            {team=attackie.team;hp=newHp;attack=attackie.attack;x=attackie.x;y=attackie.y;id=attackie.id}
        else
            {team='.';hp=0;attack=0;x=attackie.x;y=attackie.y;id= -1}

    let move (grid:Square array[]) (unit:Square) (position:int*int) =
        let newSquare = {team=unit.team;hp=unit.hp;attack=unit.attack;x=fst position;y=snd position;id=unit.id}
        let oldSquare = {team='.';hp=0;attack=0;x=unit.x;y=unit.y;id= -1}
        grid.[newSquare.y].[newSquare.x] <- newSquare
        grid.[oldSquare.y].[oldSquare.x] <- oldSquare
        newSquare

    let getTargets (unit:Square) (grid:Square array[]) =
        let targetTeam = if unit.team = 'E' then 'G' else 'E'
        grid |> Seq.concat |> Seq.filter (fun s -> s.team = targetTeam) |> Seq.toList

    let getTargetInRange (unit:Square) (grid:Square array[]) =
        let targetTeam = if unit.team = 'E' then 'G' else 'E'
        let potentialPoints = getNeighborPoints unit
        let enemies = potentialPoints |> Array.map (fun p -> grid.[snd p].[fst p]) |> Array.filter (fun s -> s.team = targetTeam) |> Array.sortBy (fun s -> (s.hp,s.y,s.x))
        Seq.tryHead enemies

    let getPath (grid:Square array[]) (unit:Square) (goal:Square) =
        let maxX = grid.[0].Length
        let maxY = grid.Length
        let asNode = {cost=0.0;square=unit}
        let frontier = new SortedSet<Node>([|asNode|])
        let mutable cameFrom = [(unit,unit)] |> Map.ofList
        let mutable alsoCameFrom = [(unit,unit)] |> Map.ofList
        let mutable costSoFar = [(unit,0.0)] |> Map.ofList
        let mutable keepGoing = true
        while keepGoing do
            if frontier.Count = 0 then
                keepGoing <- false
            else            
                let current = frontier.Min
                frontier.Remove current |> ignore
                if current.square = goal then
                    keepGoing <- false
                else
                    for next in getNeighborSpaces current.square grid do
                        let previousCost =
                            match costSoFar.TryFind current.square with
                            | Some (c) -> c
                            | None -> 0.0
                        let newCost = previousCost + 1.0
                        let theCostSoFar =
                            match costSoFar.TryFind next with
                            | Some (c) -> c
                            | None -> 10000000000.0
                        if newCost < theCostSoFar then
                            costSoFar <- costSoFar.Add (next,newCost)
                            let priority =  newCost + ((float next.y) / (float maxY)) + ((float next.x) / (float maxX * 10.0))
                            let asNode = {cost=priority;square=next}
                            frontier.Add(asNode) |> ignore
                            cameFrom <- cameFrom.Add (next,current.square)

        let mutable current = goal
        let mutable previous = None
        if cameFrom.ContainsKey goal then
            while current <> unit do
                previous <- Some current
                current <- (Map.find current cameFrom)
        if previous.IsNone then
            None
        else                     
            Some (previous.Value,Map.find goal costSoFar, goal)

   
    let first (input) =
        match input with
        | (a,b,c) -> a
    
    let second (input) =
        match input with
        | (a,b,c) -> b

    let third (input) =
        match input with
        | (a,b,c) -> c

    let getMoveTarget (unit:Square) (grid:Square array[]) (units:Square list) =
        let targetTeam = if unit.team = 'E' then 'G' else 'E'
        let targets = units |> List.filter (fun s -> s.team = targetTeam)
        if targets.Length = 0 then
            None
        else
            let targetOpenSpaces = targets |> Seq.collect (fun s -> getNeighborSpaces s grid)
            let options = targetOpenSpaces |> Seq.map (getPath grid unit) |> Seq.filter (fun s -> s.IsSome) |> Seq.map (fun s -> s.Value) |> Seq.sortBy (fun p -> (second p,(third p).y,(third p).x))
            Seq.tryHead options

    let isDead (grid:Square array[]) (unit:Square) =
        grid.[unit.y].[unit.x].hp = 0

    let printGrid (grid:Square array[]) =
        for line in grid do
            line |> Array.map (fun s -> s.team) |> String |> printfn "%A"
        // grid |> Seq.concat |> Seq.filter (fun s -> s.team <> '.' && s.team <> '#') |> Seq.map (fun s -> (s.team,s.hp,s.y,s.x)) |> Seq.toList |> printfn "%A"

    let part1 input (printRound:int) = 
        let grid = readLines input |> parseGrid 3
        let mutable combatDone = false
        let mutable rounds = 0
        let units = grid |> Seq.concat |> Seq.filter (fun s -> s.team <> '.' && s.team <> '#') |> Seq.sortBy (fun s -> (s.y,s.x)) |> Seq.toList
        let mutable unitMap = units |> Seq.map (fun u -> (u.id,u)) |> Map.ofSeq
        let mutable unitIds = units |> List.map (fun u -> u.id)
        while not combatDone do
            unitIds <- unitIds |> List.filter (fun u -> (u <> -1 ) && (Map.containsKey u unitMap)) |> List.map (fun u -> (Map.find u unitMap)) |> List.sortBy (fun u -> (u.y,u.x)) |> List.map (fun u -> u.id)
            for unitId in unitIds do
                let unitMaybe = Map.tryFind unitId unitMap
                if unitMaybe.IsSome then
                    let unit = unitMaybe.Value
                    let enemies = getTargets unit grid
                    if Seq.isEmpty enemies then
                        combatDone <- true
                    let targetInRange = getTargetInRange unit grid
                    if targetInRange.IsSome then
                        let target = targetInRange.Value
                        let target' = attack unit target
                        grid.[target.y].[target.x] <- target'
                        if target'.id = -1 then
                            unitIds <- unitIds |> List.filter (fun id -> id <> target.id)
                            unitMap <- unitMap.Remove(target.id)
                        else
                            unitMap <- unitMap.Add(target'.id,target')
                    else
                        let unitList = unitIds |> List.map (fun uid -> Map.find uid unitMap)
                        let moveTarget = getMoveTarget unit grid unitList
                        if moveTarget.IsSome then
                            let moveS = first moveTarget.Value
                            let unit' = move grid unit (moveS.x,moveS.y)
                            unitMap <- unitMap.Add(unit'.id,unit')
                            let targetInRange = getTargetInRange unit' grid
                            if targetInRange.IsSome then
                                let target = targetInRange.Value
                                let target' = attack unit' target
                                grid.[target.y].[target.x] <- target'
                                if target'.id = -1 then
                                    unitIds <- unitIds |> List.filter (fun id -> id <> target.id)
                                    unitMap <- unitMap.Remove(target.id)
                                else
                                    unitMap <- unitMap.Add(target'.id,target')
                                // units <- grid |> Seq.concat |> Seq.filter (fun s -> s.team <> '.' && s.team <> '#') |> Seq.sortBy (fun s -> (s.y,s.x)) |> Seq.toList
            rounds <- rounds + 1
            // if rounds = printRound then
            printGrid grid
            rounds |> printfn "%A"
                // combatDone <- true
        rounds |> printfn "%A"
        printGrid grid
        let totalHp = grid |> Seq.concat |> Seq.filter (fun s -> s.team <> '.' && s.team <> '#') |> Seq.sumBy (fun s -> s.hp)
        totalHp |> printfn "%A"
        totalHp * (rounds - 1)

    let part2 input (elfStrength:int) = 
        let grid = readLines input |> parseGrid elfStrength
        let mutable combatDone = false
        let mutable elfDead = false
        let mutable rounds = 0
        let units = grid |> Seq.concat |> Seq.filter (fun s -> s.team <> '.' && s.team <> '#') |> Seq.sortBy (fun s -> (s.y,s.x)) |> Seq.toList
        let mutable unitMap = units |> Seq.map (fun u -> (u.id,u)) |> Map.ofSeq
        let mutable unitIds = units |> List.map (fun u -> u.id)
        while not combatDone && not elfDead do
            unitIds <- unitIds |> List.filter (fun u -> (u <> -1 ) && (Map.containsKey u unitMap)) |> List.map (fun u -> (Map.find u unitMap)) |> List.sortBy (fun u -> (u.y,u.x)) |> List.map (fun u -> u.id)
            for unitId in unitIds do
                let unitMaybe = Map.tryFind unitId unitMap
                if unitMaybe.IsSome then
                    let unit = unitMaybe.Value
                    let enemies = getTargets unit grid
                    if Seq.isEmpty enemies then
                        combatDone <- true
                    let targetInRange = getTargetInRange unit grid
                    if targetInRange.IsSome then
                        let target = targetInRange.Value
                        let target' = attack unit target
                        grid.[target.y].[target.x] <- target'
                        if target'.id = -1 then
                            if target.team = 'E' then
                                elfDead <- true
                            unitIds <- unitIds |> List.filter (fun id -> id <> target.id)
                            unitMap <- unitMap.Remove(target.id)
                        else
                            unitMap <- unitMap.Add(target'.id,target')
                    else
                        let unitList = unitIds |> List.map (fun uid -> Map.find uid unitMap)
                        let moveTarget = getMoveTarget unit grid unitList
                        if moveTarget.IsSome then
                            let moveS = first moveTarget.Value
                            let unit' = move grid unit (moveS.x,moveS.y)
                            unitMap <- unitMap.Add(unit'.id,unit')
                            let targetInRange = getTargetInRange unit' grid
                            if targetInRange.IsSome then
                                let target = targetInRange.Value
                                let target' = attack unit' target
                                grid.[target.y].[target.x] <- target'
                                if target'.id = -1 then
                                    unitIds <- unitIds |> List.filter (fun id -> id <> target.id)
                                    unitMap <- unitMap.Remove(target.id)
                                else
                                    unitMap <- unitMap.Add(target'.id,target')
                                // units <- grid |> Seq.concat |> Seq.filter (fun s -> s.team <> '.' && s.team <> '#') |> Seq.sortBy (fun s -> (s.y,s.x)) |> Seq.toList
            rounds <- rounds + 1
            // if rounds = printRound then
            printGrid grid
            rounds |> printfn "%A"
                // combatDone <- true
        if not elfDead then
            let totalHp = grid |> Seq.concat |> Seq.filter (fun s -> s.team <> '.' && s.team <> '#') |> Seq.sumBy (fun s -> s.hp)
            totalHp |> printfn "%A"
            totalHp * (rounds - 1) |> printfn "%A"
        else
            "BAD" |> printfn "%A"