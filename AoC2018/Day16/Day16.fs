namespace AoC2018
open System.IO
open System.Text.RegularExpressions

module Day16 =
    type Sample = {start:int array;code:int*(int array);result:int array}

    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseRegisters input =
        match input with
        | Regex @".*\[(\d), (\d), (\d), (\d)]" [a;b;c;d] -> [|int a;int b;int c;int d|]
        | _ -> [||]

    let parseCode input =
        match input with
        | Regex @"(\d+) (\d) (\d) (\d)" [op;a;b;c] -> (int op,[|int a;int b;int c|])
        | _ -> (-1,[||])

    let parseSample (input:string list) =
        {start=parseRegisters input.[0];code=parseCode input.[1];result=parseRegisters input.[2]}

    let opi (op) (start:int array) (instructions:int array) =
        let result = Array.copy start
        result.[instructions.[2]] <- (op) start.[instructions.[0]] instructions.[1]
        result

    let opr (op) (start:int array) (instructions:int array) =
        let result = Array.copy start
        result.[instructions.[2]] <- (op) start.[instructions.[0]] start.[instructions.[1]]
        result

    let comir (op) (start:int array) (instructions:int array) =
        let result = Array.copy start
        result.[instructions.[2]] <- if (op) instructions.[0] start.[instructions.[1]] then 1 else 0
        result

    let comri (op) (start:int array) (instructions:int array) =
        let result = Array.copy start
        result.[instructions.[2]] <- if (op) start.[instructions.[0]] instructions.[1] then 1 else 0
        result

    let comrr (op) (start:int array) (instructions:int array) =
        let result = Array.copy start
        result.[instructions.[2]] <- if (op) start.[instructions.[0]] start.[instructions.[1]] then 1 else 0
        result

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
        let result = Array.copy start
        result.[instructions.[2]] <- instructions.[0]
        result

    let setr (start:int array) (instructions:int array) =
        let result = Array.copy start
        result.[instructions.[2]] <- start.[instructions.[0]]
        result

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

    let allOps = [|addi;addr;muli;mulr;bani;banr;bori;borr;seti;setr;gtir;gtri;gtrr;eqir;eqri;eqrr|]

    let getCount (sample:Sample) =
        allOps |> Seq.filter (fun o -> (o sample.start (snd sample.code)) = sample.result) |> Seq.length

    let mutable opCodeMap = [(-1,-1)] |> Map.ofList
    let mutable foundOpCodes = []

    let findMatchIndex (sample:Sample) =
        let matchMaybe = Map.tryFind (fst sample.code) opCodeMap
        if matchMaybe.IsSome then
            matchMaybe
        else
            let options = [0..15] |> Seq.filter (fun i -> not (Seq.contains i foundOpCodes))
            let mapped = options |> Seq.map (fun i -> (i,allOps.[i])) |> Seq.map (fun io -> 
                let o = snd io
                let i = fst io
                (i,(o sample.start (snd sample.code)) = sample.result)) |> Seq.filter snd
            if Seq.length mapped > 1 then
                None
            else
                // We found a match
                let index = Seq.head mapped |> fst
                opCodeMap <- opCodeMap.Add (fst sample.code, index)
                foundOpCodes <- List.append foundOpCodes [index]
                Some index

            

    let part1 input =
        let samples = readLines input |> Seq.filter (fun s -> s.Length > 0) |> Seq.toList |> List.chunkBySize 3 |> List.map parseSample
        samples |> Seq.length |> printfn "%A"
        samples |> Seq.head |> printfn "%A"
        samples |> Seq.map getCount |> Seq.filter (fun c -> c = 1) |> Seq.length

    let part2Prep input =
        let samples = readLines input |> Seq.filter (fun s -> s.Length > 0) |> Seq.toList |> List.chunkBySize 3 |> List.map parseSample
        let solvable = samples |> Seq.filter (fun s -> getCount s = 8) |> Seq.filter (fun s -> (Map.tryFind (fst s.code) opCodeMap).IsNone)
        solvable |> Seq.map findMatchIndex |> Seq.filter (fun i -> i.IsSome)

    let part2 input =
        let lines = readLines input |> Seq.map parseCode
        let mutable registers = [|0;0;0;0|]
        lines |> Seq.fold (fun start (o,d) -> 
            let op = allOps.[Map.find o opCodeMap]
            op start d
            ) registers