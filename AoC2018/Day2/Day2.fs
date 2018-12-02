namespace AoC2018
open System.IO

module Day2 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let countChar x = Seq.filter ((=) x) >> Seq.length
    
    let acceptWord (word:string) (count:int) = 
        let counts = word |> Seq.map (fun x -> countChar x word)
        counts |> Seq.exists ((=) count)

    let acceptWords (word1:string) (word2:string) =
        let mutable diff = 0
        let mutable index = 0
        for i = 0 to word1.Length - 1 do
            if word1.[i] <> word2.[i] then
                diff <- diff + 1
                index <- i
        if diff = 1 then
            printfn "%d" index
        diff = 1

    let part1 (words:seq<string>) = 
        let count2 = words |> Seq.filter (fun word -> acceptWord word 2) |> Seq.length
        let count3 = words |> Seq.filter (fun word -> acceptWord word 3) |> Seq.length
        count2 * count3

    let part2 (words:seq<string>) =
        words |> Seq.filter (fun word -> words |> Seq.exists (fun word2 -> acceptWords word word2))

    
