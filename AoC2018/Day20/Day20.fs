namespace AoC2018
open System
open System.IO
open System.Collections.Generic

module Day20 =
    type ImmutableStack<'T> =
        | Empty 
        | Stack of 'T * ImmutableStack<'T>

        member s.Push x = Stack(x, s)

        member s.Peek() =
            match s with
            | Empty -> failwith "Underflow peek"
            | Stack(t,_) -> t

        member s.Pop() = 
            match s with
            | Empty -> failwith "Underflow pop"
            | Stack(t,st) -> t,st

        member s.Top() = 
            match s with
            | Empty -> ImmutableStack.Empty
            | Stack(_,st) -> st

        member s.IEmpty = 
            match s with
            | Empty -> true
            | _ -> false

        member s.All() = 
            let rec loop acc = function
            | Empty -> acc
            | Stack(t,st) -> loop (t::acc) st
            loop [] s

    let readLine (filePath:string) =
        use sr = new StreamReader(filePath)
        sr.ReadLine()

    type Room = {id:int*int;mutable north:option<Room>;mutable east:option<Room>;mutable west:option<Room>;mutable south:option<Room>;mutable distance:int}

    let safeAdd (room:Room) (map:Dictionary<(int*int),Room>) =
        if not (map.ContainsKey(room.id)) then
            map.Add(room.id, room)

    let getNew (map:Dictionary<(int*int),Room>) (id:int*int) (distance:int) =
        let newRoom = {id=id;north=None;east=None;south=None;west=None;distance=distance}
        safeAdd newRoom map
        newRoom

    let tryFind (id:int*int) (map:Dictionary<(int*int),Room>) (currentDistance:int) =
        let newDistance = currentDistance + 1
        match map.ContainsKey(id) with
        | true -> 
            let found = map.[id]
            if found.distance > newDistance then
                found.distance <- newDistance
            found
        | false -> getNew map id newDistance

    let processMove (map:Dictionary<(int*int),Room>) (current:Room) (direction:char) =
        match direction with
        | 'N' ->
            if current.north.IsSome then
                current.north.Value
            else
                // See if it does exist
                let id = (fst current.id,(snd current.id) - 1)
                let connect = tryFind id map current.distance
                connect.south <- Some current
                current.north <- Some connect
                connect
        | 'E' ->
            if current.east.IsSome then
                current.east.Value
            else
                // See if it does exist
                let id = ((fst current.id) + 1,(snd current.id))
                let connect = tryFind id map current.distance
                connect.west <- Some current
                current.east <- Some connect
                connect
        | 'W' ->
            if current.west.IsSome then
                current.west.Value
            else
                // See if it does exist
                let id = ((fst current.id) - 1,(snd current.id))
                let connect = tryFind id map current.distance
                connect.east <- Some current
                current.west <- Some connect
                connect
        | 'S' ->
            if current.south.IsSome then
                current.south.Value
            else
                // See if it does exist
                let id = ((fst current.id),(snd current.id) + 1)
                let connect = tryFind id map current.distance   
                connect.north <- Some current
                current.south <- Some connect
                connect
        | _ -> failwith ("Encountered not NSEW, but " + String [|direction|])

    // Assume we're givin the rest of the word past the star paren
    let findEndParen (input:string) =
        let mutable needCount = 1
        let mutable index = -1
        for (i, char) in input |> Seq.indexed  do
            if index = -1 then
                if char = '(' then
                    needCount <- needCount + 1
                if char = ')' then
                    needCount <- needCount - 1
                    if needCount = 0 then
                        index <- i
        index

    let rec processWord (map:Dictionary<(int*int),Room>) (current:Room) (word:string) =
        // word |> printfn "%A"
        // current.id |> printfn "%A"
        let rec processBranch (map:Dictionary<(int*int),Room>) (current:Room) (word:string) =
            // word |> printfn "BRANCH %A"
            // current.id |> printfn "%A"
            let branchIndex = word.IndexOf('|')
            let words = [|word.[0..branchIndex - 1];word.[branchIndex + 1..]|]
            let items = words |> Seq.map (processWord map current) |> Seq.concat |> Seq.distinctBy (fun r -> r.id)
            let toReturn = new List<Room>()
            toReturn.AddRange(items)
            // items |> Seq.map (fun r -> r.id) |> printfn "%A"
            toReturn

        let toReturn = new List<Room>()
        if word.Length = 0 then
            toReturn.Add(current)
            toReturn
        else
            let mutable current' = current
            let startBranchIndex = word.IndexOf('|')
            let startParenIndex = word.IndexOf("(")
            if startBranchIndex > -1 && (startBranchIndex < startParenIndex || startParenIndex = -1) then
                processBranch map current word
            else 
                if startParenIndex > -1 then
                    let endParenIndex = findEndParen word.[startParenIndex + 1..] + startParenIndex + 1
                    if startParenIndex > 0 then
                        current' <- (processWord map current' word.[0..startParenIndex - 1]).[0]
                    let toFigureOut = new List<Room>()
                    toFigureOut.AddRange(processWord map current' word.[startParenIndex + 1..endParenIndex - 1])
                    if endParenIndex < word.Length - 1 then
                        for possible in toFigureOut do
                            toReturn.AddRange(processWord map possible word.[endParenIndex + 1..])
                        toReturn
                    else
                        toReturn
                else
                    for letter in word do
                        current' <- processMove map current' letter
                    toReturn.Add(current')
                    toReturn

    let printGrid (grid:char array[]) =
        for line in grid do
            line |> String |> printfn "%A"

    let printMap (map:Dictionary<(int*int),Room>) =
        let allPoints = map.Values |> Seq.map (fun r -> r.id)
        let minX = allPoints |> Seq.minBy fst |> fst
        let maxX = allPoints |> Seq.maxBy fst |> fst
        let minY = allPoints |> Seq.minBy snd |> snd
        let maxY = allPoints |> Seq.maxBy snd |> snd
        let xOffset = 0 - minX
        let yOffset = 0 - minY
        let ySize = (maxY - minY + 1) * 2 + 1
        let xSize = (maxX - minX + 1) * 2 + 1
        let grid = Array.init ySize (fun i -> (Array.create xSize '#'))
        for (KeyValue(id,room)) in map do
            let roomX = ((fst id) + xOffset) * 2 + 1
            let roomY = ((snd id) + yOffset) * 2 + 1
            if id = (0,0) then
                grid.[roomY].[roomX] <- 'x'
            else
                grid.[roomY].[roomX] <- '.'
            if room.east.IsSome then
                grid.[roomY].[roomX + 1] <- '|'
            if room.south.IsSome then
                grid.[roomY + 1].[roomX] <- '-'
            if room.west.IsSome then
                grid.[roomY].[roomX - 1] <- '|'
            if room.north.IsSome then
                grid.[roomY - 1].[roomX] <- '-'

        printGrid grid

    
    let part1ViaStack input =
        let instructions = readLine input
        let map = new Dictionary<int*int, Room>()
        let mutable current = getNew map (0,0) 0
        let stackStart = ImmutableStack.Empty
        let mutable stack = stackStart.Push current
        for letter in instructions do
            if letter = '(' then
                stack <- stack.Push(current)
            elif letter = '|' then
                current <- stack.Peek()
            elif letter = ')' then
                let (current',stack') = stack.Pop()
                current <- current'
                stack <- stack'
            else
                current <- processMove map current letter
        
        // printMap map
        let max = map.Values |> Seq.maxBy (fun r -> r.distance)
        max.distance


    let part2ViaStack input =
        let instructions = readLine input
        let map = new Dictionary<int*int, Room>()
        let mutable current = getNew map (0,0) 0
        let stackStart = ImmutableStack.Empty
        let mutable stack = stackStart.Push current
        for letter in instructions do
            if letter = '(' then
                stack <- stack.Push(current)
            elif letter = '|' then
                current <- stack.Peek()
            elif letter = ')' then
                let (current',stack') = stack.Pop()
                current <- current'
                stack <- stack'
            else
                current <- processMove map current letter
        
        // printMap map
        map.Values |> Seq.map (fun r -> r.distance) |> Seq.filter (fun d -> d >= 1000) |> Seq.length

    let part1 input = 
        let instructions = readLine input
        let map = new Dictionary<int*int, Room>()
        let mutable current = getNew map (0,0) 0
        processWord map current instructions |> ignore
        // printMap map
        let max = map.Values |> Seq.maxBy (fun r -> r.distance)
        max.distance