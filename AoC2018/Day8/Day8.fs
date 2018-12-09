namespace AoC2018
open System.IO

module Day8 =
    let (|EmptySeq|_|) a = if Seq.isEmpty a then Some () else None

    type Node = | NodeContents of (int list * Node list)

    // type NodeOther = | NodeContentsOther of (int list * NodeOther list)

    let readLine (filePath:string) =
        use sr = new StreamReader(filePath)
        sr.ReadLine()

    let lastN n xs =
        let rec skip n xs = 
            match n, xs with
            | _, []     -> []   // empty list, returning unchanged
            | 0, _      -> xs   // found an element at which the remainder
                                // of the list is to be returned
            | n', h::t  -> skip (n-1) t    // proceed to next iteration

        let toSkip = (List.length xs) - n  // how many elements to skip
        if toSkip < 0 then xs   // or an exception, depending on expected behavior
        elif toSkip = 0 then xs // requested exactly as many elements
                                // as the list contains
        else skip toSkip xs

    let parseInput (line:string) =
        line.Split [|' '|] |> Seq.map int

    let pop (input) =
        (Seq.head input, Seq.tail input)

    let rec getNodesOther (numbers:int list) =
        let mutable workingNumbers = numbers
        let (childCount, numbers') = pop numbers
        let (metaCount, numbers') = pop numbers'
        workingNumbers <- numbers' |> Seq.toList
        let mutable children = []
        let mutable metaNums = []
        for i = 1 to childCount do
            let (children', numbers') = getNodesOther workingNumbers
            workingNumbers <- numbers'
            children <- Seq.append children children' |> Seq.toList
        for j = 1 to metaCount do 
            let (meta, numbers') = pop workingNumbers
            workingNumbers <- numbers' |> Seq.toList
            metaNums <- meta::metaNums
        metaNums |> Seq.toList |> printfn "metafinal %A"
        ([NodeContents (Seq.toList metaNums,children)], workingNumbers)
        


    // let rec getNodes (count:int) (numbers:seq<int>) =
    //     let childCount = Seq.head numbers
    //     let metaCount = Seq.skip 1 numbers |> Seq.head
    //     if childCount = 0 then
    //         let metaNums = Seq.skip 2 numbers |> Seq.take metaCount |> Seq.toList
    //         if count > 1 then
    //             let rest = Seq.skip (2 + metaCount) numbers |> getNodes (count - 1)
    //             Seq.append [NodeContents (metaNums, Seq.empty<Node>)] rest
    //         else
    //             Seq.ofList [NodeContents (metaNums, Seq.empty<Node>)]
    //     else
    //         let metaNums = numbers |> Seq.toList |> lastN metaCount
    //         let restCount = (Seq.length numbers) - metaCount - 2
    //         Seq.ofList [NodeContents (metaNums, getNodes childCount (numbers |> Seq.skip 2 |> Seq.take restCount))]

    let nodeFst (node:Node) =
        let (NodeContents contents) = node
        match contents with
        | (a,b) -> a

    let nodeSnd (node:Node) =
        let (NodeContents contents) = node
        match contents with
        | (a,b) -> b

    let rec part2Sum (node:Node) =
        let children = nodeSnd node
        if Seq.isEmpty children then
            nodeFst node |> Seq.sum
        else
            nodeFst node |> Seq.map (fun index -> 
                if index = 0 || index > children.Length then
                    0
                else
                    part2Sum children.[index - 1]
            ) |> Seq.sum

    let rec sumMeta (nodes:Node list) =
        let sumOfNode (node:Node) =
            let metaSum = nodeFst node |> Seq.sum
            printfn "%A" metaSum
            let restSum = nodeSnd node |> sumMeta
            printfn "%A" restSum
            metaSum + restSum

        if Seq.isEmpty nodes then
            0
        else
            nodes |> Seq.sumBy sumOfNode

    let part1 (input) =
        let (nodes, _) = readLine input |> parseInput |> Seq.toList |> getNodesOther
        nodes |> sumMeta

    let part2 (input) =
        let (nodes, _) = readLine input |> parseInput |> Seq.toList |> getNodesOther
        nodes |> Seq.sumBy part2Sum