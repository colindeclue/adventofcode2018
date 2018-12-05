namespace AoC2018
open System
open System.IO
open System.Collections.Generic

module Day5 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let compareChars (input:char[]) =
        input.[0] <> input.[1] &&
        (input.[0] |> Char.ToUpper = input.[1] ||
         input.[1] |> Char.ToUpper = input.[0])


    let getRemoveCharacters (input:string) = 
        Seq.windowed 2 input |> Seq.filter compareChars |> Seq.map String

    let reduceCollisions (input:string) =
        let removeCharacters = input |> getRemoveCharacters
        let mutable output:string = input
        if Seq.isEmpty removeCharacters then
            output <- input
        else
            for replace in removeCharacters do
                output <- output.Replace(replace, "")
        output

    let fullReduce (input:string) =
        let mutable prevLength = input.Length
        let mutable reduce = reduceCollisions input
        let mutable currentLength = reduce.Length
        while prevLength <> currentLength do
            prevLength <- reduce.Length
            reduce <- reduceCollisions reduce
            currentLength <- reduce.Length
        
        reduce

    

    let peek list =
        Seq.head list

    let pop list =
        match list with
        | top::rest ->
            (top,rest)
        | [] -> failwith "Stack underflow"

    let push x list =
        x::list

    let fullReduceFaster word =
        let stack:Stack<char> = new Stack<char>()

        for char in word do
            if Seq.length stack = 0 then
                stack.Push(char)
            else
                let top = stack.Peek()
                if not (compareChars [|top; char|]) then
                    stack.Push(char)
                else
                    stack.Pop() |> ignore
                    
                
        stack.Count

    let part1 (input) =
        readLines input |> Seq.head |> fullReduce |> Seq.length

    let part1Fast (input) =
        readLines input |> Seq.head |> fullReduceFaster

    let withoutChar (input:string) (c:char) =
        input.Replace(c,' ').Replace(c |> Char.ToUpper,' ').Replace(" ","") |> fullReduce |> Seq.length

    let part2 (input) =
        let polymer = readLines input |> Seq.head
        let without = withoutChar polymer
        let counts = ['a'..'z'] |> Seq.map without
        Seq.min counts
