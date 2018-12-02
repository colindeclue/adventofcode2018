namespace AoC2018
open System.IO
open System

module Day2 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let countChar (word) (x) =  word |> Seq.filter ((=) x) |> Seq.length
    
    let acceptWord (count) (word) = 
        let counts = word |> Seq.map (countChar word)
        counts |> Seq.exists ((=) count)

    let acceptWords (word1:string) (word2:string) =
        let mutable diff = 0
        let mutable index = 0
        for i = 0 to word1.Length - 1 do
            if word1.[i] <> word2.[i] then
                diff <- diff + 1
                index <- i
        diff = 1

    let part1 (words:seq<string>) = 
        let count2 = words |> Seq.filter (acceptWord 2) |> Seq.length
        let count3 = words |> Seq.filter (acceptWord 3) |> Seq.length
        count2 * count3

    let part2 (words:seq<string>) =
        let almostMatch = words |> Seq.filter (fun word -> words |> Seq.exists (acceptWords word)) |> Seq.toList
        Seq.zip almostMatch.[0] almostMatch.[1] |> Seq.filter (fun (f, s) -> f = s) |> Seq.map (fun (f,s) -> f) |> Seq.toArray |> String
