namespace AoC2018
open System.IO

module Day1 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let findMatch (set:byref<Set<int>>) (changes:seq<int>) (sum:byref<int>) =
        let mutable answer = 0
        for change in changes do
            sum <- sum + change
            if(set.Contains(sum)) then 
                answer <- sum
            else 
                set <- set.Add(sum)
        match answer with
            | 0 -> (true, sum)
            | _ -> (false, answer)
            
    let shouldContinue (set:Set<int>) (i:int) (changes:List<int>) =
        set.Contains(changes.[i])

    let stringToSeq(input:string) =
        input.Split [|','|] |> Seq.map(int)

    let solution(input:string) = 
        let mutable set = Set.empty.Add(0)
        let mutable frequency = 0
        let changes = readLines input |> Seq.map(int)
        let mutable keepGoing = true
        let mutable answer = 0

        while keepGoing do
            let stillHappens, sum = findMatch &set changes &frequency
            if(set.Contains(answer)) then
                keepGoing <- false
            
        
        answer
                