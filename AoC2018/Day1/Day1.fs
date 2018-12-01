namespace AoC2018
open System.IO

module Day1 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let shouldContinue (set:Set<int>) (i:int) (changes:List<int>) =
        set.Contains(changes.[i])

    let stringToSeq(input:string) =
        input.Split [|','|] |> Seq.map(int)

    let solution(input:string) = 
        let mutable set = Set.empty.Add(0)
        let mutable frequency = 0
        let changes = readLines input |> Seq.map(int) |> Seq.toList
        let mutable keepGoing = true
        let mutable count = 1
        while keepGoing do
            count <- count + 1
            for change in changes do
                frequency <- frequency + change
                if(set.Contains frequency) then
                    keepGoing <- false
                    printfn "%d" frequency
                else
                    set <- set.Add(frequency)
        
        count
                