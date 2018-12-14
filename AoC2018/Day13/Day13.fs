namespace AoC2018
open System.IO

module Day13 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    type Direction = Up=0|Right=1|Down=2|Left=3
    type Heading = Left=0|Straight=1|Right=2

    // Headings will all start at left
    type Cart = {x:int;y:int;direction:Direction;heading:Heading;id:int}

    let cartSort (cart:Cart) =
        (cart.y,cart.x)

    let tryGetCart (value:char) (point:int*int) (id:int) =
        match value with
        | 'v' ->  (Some{x=fst point; y=snd point;direction=Direction.Down;heading=Heading.Left;id=id},'|')
        | '>' ->  (Some{x=fst point; y=snd point;direction=Direction.Right;heading=Heading.Left;id=id},'-')
        | '^' ->  (Some{x=fst point; y=snd point;direction=Direction.Up;heading=Heading.Left;id=id},'|')
        | '<' ->  (Some{x=fst point; y=snd point;direction=Direction.Left;heading=Heading.Left;id=id},'|')
        | c -> (None,c)

    let tryGetCrash (carts:seq<Cart>) =
        let byX = carts |> Seq.sortBy (fun c -> (c.x,c.y))
        let byY = carts |> Seq.sortBy (fun c -> (c.y,c.x))
        let mutable crashes = Seq.windowed 2 byX |> Seq.filter (fun x -> x.[0].x = x.[1].x && x.[0].y = x.[1].y) |> Seq.map (fun x -> x.[0])
        if Seq.isEmpty crashes then
            crashes <- Seq.windowed 2 byY |> Seq.filter (fun x -> x.[0].x = x.[1].x && x.[0].y = x.[1].y) |> Seq.map (fun x -> x.[0])
        if not (Seq.isEmpty crashes) then
            if Seq.length crashes > 1 then "Multiple crashes" |> printfn "%A"
            let cart = Seq.head crashes
            Some (cart.x,cart.y)
        else
            None

    let parseGrid (lines:seq<string>) =
        let mutable y = 0
        let mutable x = 0
        let mutable carts = Array.empty
        let mutable grid = Array.empty<array<char>>
        let mutable id = 0
        for line in lines do
            grid <- Array.append grid [|(Array.create line.Length ' ')|]
            for char in line do
                let (cart,value) = tryGetCart char (x,y) id
                grid.[y].[x] <- value
                if cart.IsSome then
                    id <- id + 1
                    carts <- Array.append carts [|cart.Value|]
                x <- x + 1
            y <- y + 1
            x <- 0
        (grid,carts |> Seq.sortBy cartSort)

    let moveLeftOrRight (cart:Cart) =
        let y = cart.y;
        let x = cart.x;
        let delta = if cart.direction = Direction.Right then 1 else -1
        {x=x + delta;y=y;direction=cart.direction;heading=cart.heading;id=cart.id}

    let getXY (cart:Cart) =
        let mutable x = cart.x
        let mutable y = cart.y
        let mutable deltaX = 0
        let mutable deltaY = 0
        if cart.direction = Direction.Left then
            deltaX <- -1
        elif cart.direction = Direction.Right then
            deltaX <- 1
        elif cart.direction = Direction.Up then
            deltaY <- -1
        elif cart.direction = Direction.Down then
            deltaY <- 1
        (x + deltaX, y + deltaY)
    
    let getDirectionAndHeading (char:char) (cart:Cart) =
        let mutable effectiveChar = char
        let mutable heading = cart.heading
        if effectiveChar = '+' then
            effectiveChar <- 
                match cart.heading with
                | Heading.Left -> 
                    heading <- Heading.Straight
                    match cart.direction with
                    | Direction.Up -> '\\'
                    | Direction.Right -> '/'
                    | Direction.Down -> '\\'
                    | Direction.Left -> '/'
                    | _ -> failwith "Bad direction"
                | Heading.Straight -> 
                    heading <- Heading.Right
                    '|'
                | Heading.Right ->
                    heading <- Heading.Left
                    match cart.direction with
                    | Direction.Up -> '/'
                    | Direction.Right -> '\\'
                    | Direction.Down -> '/'
                    | Direction.Left -> '\\'
                    | _ -> failwith "Bad direction"
                | _ -> failwith "bad heading"                
        if effectiveChar = '|' || effectiveChar = '-' then
            (cart.direction,heading)
        elif effectiveChar = '/' then
            let direction = 
                match cart.direction with
                | Direction.Up -> Direction.Right
                | Direction.Right -> Direction.Up
                | Direction.Down -> Direction.Left
                | Direction.Left -> Direction.Down
                | _ -> failwith "Bad direction"
            (direction,heading)
        else
            let direction = 
                match cart.direction with
                | Direction.Up -> Direction.Left
                | Direction.Right -> Direction.Down
                | Direction.Down -> Direction.Right
                | Direction.Left -> Direction.Up
                | _ -> failwith "Bad direction"
            (direction,heading)


    let tick (grid:char array[]) (cart:Cart) =
        let (x,y) = getXY cart
        let track = grid.[y].[x]
        let (direction,heading) = getDirectionAndHeading track cart
        {x=x;y=y;direction=direction;heading=heading;id=cart.id}

    let crash (cart1:Cart) (cart2:Cart) =
        cart1.x = cart2.x && cart1.y = cart2.y

    let part1 input =
        let (grid,carts) = readLines input |> parseGrid
        let mutable carts' = carts |> Seq.toList
        let mutable foundCrash = false
        let ticker = tick grid
        while not foundCrash do
            let mutable nextTickCarts = List.empty<Cart>
            for cart in carts' do
                let newCart = ticker cart
                let isCrash = crash newCart
                let mutable crashes = carts' |> Seq.filter isCrash |> Seq.toList
                crashes <- List.append crashes nextTickCarts |> Seq.filter isCrash |> Seq.toList
                if not (Seq.isEmpty crashes) then
                    Seq.head crashes |> printfn "%A"
                    foundCrash <- true
                else
                    nextTickCarts <- List.append nextTickCarts [newCart]

            carts' <- nextTickCarts |> Seq.sortBy cartSort |> Seq.toList


    let part2 input =
        let (grid,carts) = readLines input |> parseGrid
        let mutable carts' = carts |> Seq.toList
        let ticker = tick grid
        let mutable count = carts'.Length
        while carts'.Length > 1 do
            let mutable nextTickCarts = List.empty<Cart>
            let mutable removeCarts = List.empty<Cart>
            for cart in carts' do
                if not (List.exists (fun x -> x.id = cart.id) removeCarts) then
                    let newCart = ticker cart
                    let isCrash = crash newCart
                    let mutable crashes = carts' |> List.filter (fun x -> not (List.exists (fun y -> x.id = y.id) removeCarts)) |> List.filter (fun x -> not (List.exists (fun y -> x.id = y.id) nextTickCarts)) |> Seq.filter isCrash |> Seq.toList
                    let newCrashes = nextTickCarts |> Seq.filter isCrash |> Seq.toList
                    crashes <- List.append crashes newCrashes
                    if not (Seq.isEmpty crashes) then
                        carts' <- carts' |> List.filter (fun x -> x.id <> crashes.[0].id)
                        removeCarts <- List.append crashes removeCarts |> List.append [newCart]
                        nextTickCarts <- nextTickCarts |> List.filter (fun x -> not (List.exists (fun y -> x.id =y.id) removeCarts))
                    else
                        nextTickCarts <- List.append nextTickCarts [newCart]
                else
                    "yay" |> printfn "%A"

            carts' <- nextTickCarts |> List.sortBy cartSort
            if Seq.length carts' < count then
                count <- Seq.length carts'
                count |> printfn "%A"
                removeCarts |> List.map (fun x -> x.id) |> printfn "%A"
                carts' |> List.map (fun x -> x.id) |> printfn "%A"

        carts'