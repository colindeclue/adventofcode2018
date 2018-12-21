namespace AoC2018
open System.Collections.Generic
open System.Text.RegularExpressions
open System.IO

module Day21 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseInstruction input = 
        match input with
        | Regex @"(\w+) (\d+) (\d+) (\d+)" [op;a;b;c] -> (op,[|int a;int b;int c|])
        | _ -> ("fake",[|1;2;3|])

    let opi (op) (start:int array) (instructions:int array) =
        start.[int instructions.[2]] <- (op) start.[int instructions.[0]] instructions.[1]
        if instructions.[2] = 0 then
            start |> printfn "%A"

    let opr (op) (start:int array) (instructions:int array) =
        start.[int instructions.[2]] <- (op) start.[int instructions.[0]] start.[int instructions.[1]]
        if instructions.[2] = 0 then
            start |> printfn "%A"

    let comir (op) (start:int array) (instructions:int array) =
        start.[int instructions.[2]] <- if (op) instructions.[0] start.[int instructions.[1]] then 1 else 0
        if instructions.[2] = 0 then
            start |> printfn "%A"

    let comri (op) (start:int array) (instructions:int array) =
        start.[int instructions.[2]] <- if (op) start.[int instructions.[0]] instructions.[1] then 1 else 0
        if instructions.[2] = 0 then
            start |> printfn "%A"

    let comrr (op) (start:int array) (instructions:int array) =
        start.[int instructions.[2]] <- if (op) start.[int instructions.[0]] start.[int instructions.[1]] then 1 else 0
        if instructions.[2] = 0 then
            start |> printfn "%A"

    let addi (start:int array) (instructions:int array) =
        opi (+) start instructions

    let addr (start:int array) (instructions:int array) =
        opr (+) start instructions

    let muli (start:int array) (instructions:int array) =
        opi (*) start instructions

    let mulr (start:int array) (instructions:int array) =
        opr (*) start instructions

    let bani (start:int array) (instructions:int array) =
        opi (&&&) start instructions

    let banr (start:int array) (instructions:int array) =
        opr (&&&) start instructions

    let bori (start:int array) (instructions:int array) =
        opi (|||) start instructions

    let borr (start:int array) (instructions:int array) =
        opr (|||) start instructions
    
    let seti (start:int array) (instructions:int array) =
        start.[int instructions.[2]] <- instructions.[0]
        if instructions.[2] = 0 then
            start |> printfn "%A"

    let setr (start:int array) (instructions:int array) =
        start.[int instructions.[2]] <- start.[int instructions.[0]]
        if instructions.[2] = 0 then
            start |> printfn "%A"

    let gtir (start:int array) (instructions:int array) =
        comir (>) start instructions

    let gtri (start:int array) (instructions:int array) =
        comri (>) start instructions

    let gtrr (start:int array) (instructions:int array) =
        comrr (>) start instructions

    let eqir (start:int array) (instructions:int array) =
        comir (=) start instructions

    let eqri (start:int array) (instructions:int array) =
        comri (=) start instructions

    let eqrr (start:int array) (instructions:int array) =
        comrr (=) start instructions

    let operations = dict[
        "addr",addr;
        "addi",addi;
        "mulr",mulr;
        "muli",muli;
        "banr",banr;
        "bani",bani;
        "borr",borr;
        "bori",bori;
        "setr",setr;
        "seti",seti;
        "gtir",gtir;
        "gtri",gtri;
        "gtrr",gtrr;
        "eqir",eqir;
        "eqri",eqri;
        "eqrr",eqrr
    ]

    let doOp (registers:int[]) (instruction:string*int[]) =
        operations.[fst instruction] registers (snd instruction)

    let part1 input maxCount valuesSeen (registers:int array) =
        let mutable valuesSeen' = valuesSeen
        let mutable previous = 0
        let mutable count = 0
        let instructions = readLines input |> Seq.map parseInstruction |> Seq.toArray
        let mutable ip = 0
        let ipIndex = 5
        // let registers = [|1L;10551365L;1L;11L;10551364L;10551367L;|]
        while count < maxCount do
            registers.[ipIndex] <- ip
            let instruction = instructions.[ip]
            doOp registers instruction
            if ip = 29 then
                if not (Set.contains registers.[4] valuesSeen) then
                    count <- count + 1
                    previous <- registers.[4]
                    valuesSeen' <- valuesSeen'.Add(registers.[4])
                else
                    "FOUND" |> printfn "%A"
                    count <- maxCount
            // registers |> printfn "%A"
            ip <-registers.[ipIndex] + 1
        previous |> printfn "%A"
        (registers,valuesSeen')