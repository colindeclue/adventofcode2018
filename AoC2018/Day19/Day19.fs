namespace AoC2018
open System.Text.RegularExpressions
open System.IO

module Day19 =
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
        | Regex @"(\w+) (\d+) (\d+) (\d+)" [op;a;b;c] -> (op,[|int64 a;int64 b;int64 c|])
        | _ -> ("fake",[|1L;2L;3L|])

    let opi (op) (start:int64 array) (instructions:int64 array) =
        start.[int instructions.[2]] <- (op) start.[int instructions.[0]] instructions.[1]
        if instructions.[2] = 0L then
            start |> printfn "%A"

    let opr (op) (start:int64 array) (instructions:int64 array) =
        start.[int instructions.[2]] <- (op) start.[int instructions.[0]] start.[int instructions.[1]]
        if instructions.[2] = 0L then
            start |> printfn "%A"

    let comir (op) (start:int64 array) (instructions:int64 array) =
        start.[int instructions.[2]] <- if (op) instructions.[0] start.[int instructions.[1]] then 1L else 0L
        if instructions.[2] = 0L then
            start |> printfn "%A"

    let comri (op) (start:int64 array) (instructions:int64 array) =
        start.[int instructions.[2]] <- if (op) start.[int instructions.[0]] instructions.[1] then 1L else 0L
        if instructions.[2] = 0L then
            start |> printfn "%A"

    let comrr (op) (start:int64 array) (instructions:int64 array) =
        start.[int instructions.[2]] <- if (op) start.[int instructions.[0]] start.[int instructions.[1]] then 1L else 0L
        if instructions.[2] = 0L then
            start |> printfn "%A"

    let addi (start:int64 array) (instructions:int64 array) =
        opi (+) start instructions

    let addr (start:int64 array) (instructions:int64 array) =
        opr (+) start instructions

    let muli (start:int64 array) (instructions:int64 array) =
        opi (*) start instructions

    let mulr (start:int64 array) (instructions:int64 array) =
        opr (*) start instructions

    let bani (start:int64 array) (instructions:int64 array) =
        opi (&&&) start instructions

    let banr (start:int64 array) (instructions:int64 array) =
        opr (&&&) start instructions

    let bori (start:int64 array) (instructions:int64 array) =
        opi (|||) start instructions

    let borr (start:int64 array) (instructions:int64 array) =
        opr (|||) start instructions
    
    let seti (start:int64 array) (instructions:int64 array) =
        start.[int instructions.[2]] <- instructions.[0]
        if instructions.[2] = 0L then
            start |> printfn "%A"

    let setr (start:int64 array) (instructions:int64 array) =
        start.[int instructions.[2]] <- start.[int instructions.[0]]
        if instructions.[2] = 0L then
            start |> printfn "%A"

    let gtir (start:int64 array) (instructions:int64 array) =
        comir (>) start instructions

    let gtri (start:int64 array) (instructions:int64 array) =
        comri (>) start instructions

    let gtrr (start:int64 array) (instructions:int64 array) =
        comrr (>) start instructions

    let eqir (start:int64 array) (instructions:int64 array) =
        comir (=) start instructions

    let eqri (start:int64 array) (instructions:int64 array) =
        comri (=) start instructions

    let eqrr (start:int64 array) (instructions:int64 array) =
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

    let doOp (registers:int64[]) (instruction:string*int64[]) =
        operations.[fst instruction] registers (snd instruction)

    let part1 input =
        let instructions = readLines input |> Seq.map parseInstruction |> Seq.toArray
        let mutable ip = 12
        let ipIndex = 3
        // let registers = [|1L;10551365L;1L;11L;10551364L;10551367L;|]
        let registers = [|1L;2800L;1L;11L;2800L;10551367L|]
        while ip < instructions.Length do
            // if ip = 3 then
            //     registers.[4] <- registers.[5] + 1L
            //     ip <- 9
            registers.[ipIndex] <- int64 ip
            let instruction = instructions.[ip]
            doOp registers instruction
            ip <-int registers.[ipIndex] + 1
            if ip = 12 then do
                (ip,registers) |> printfn "%A"
        registers