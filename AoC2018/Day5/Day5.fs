namespace AoC2018
open System
open System.IO
open System.Collections.Generic

module Day5 =
    type ImmutableStack<'T> =
        | Empty 
        | Stack of 'T * ImmutableStack<'T>

        member s.Push x = Stack(x, s)

        member s.Peek() =
            match s with
            | Empty -> failwith "Underflow"
            | Stack(t,_) -> t

        member s.Pop() = 
            match s with
            | Empty -> failwith "Underflow"
            | Stack(t,st) -> t,st.Top()

        member s.Top() = 
            match s with
            | Empty -> failwith "Contain no elements"
            | Stack(_,st) -> st

        member s.IEmpty = 
            match s with
            | Empty -> true
            | _ -> false

        member s.All() = 
            let rec loop acc = function
            | Empty -> acc
            | Stack(t,st) -> loop (t::acc) st
            loop [] s

    let readLine (filePath:string) =
        use sr = new StreamReader(filePath)
        sr.ReadLine()

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

    let fullReduceFaster word =
        let mutable stack = ImmutableStack.Empty

        for char in word do
            if stack.IEmpty then
                stack <- stack.Push(char)
            else
                let top = stack.Peek()
                if not (compareChars [|top; char|]) then
                    stack <- stack.Push(char)
                else
                    let _,stack2 = stack.Pop()
                    stack <- stack2
                    
                
        stack.All() |> List.length

    let part1 (input) =
        readLine input |> fullReduce |> Seq.length

    let part1Fast (input) =
        readLine input |> fullReduceFaster

    let withoutChar (input:string) (c:char) =
        input.Replace(c,' ').Replace(c |> Char.ToUpper,' ').Replace(" ","") |> fullReduceFaster

    let part2 (input) =
        let polymer = readLine input
        let without = withoutChar polymer
        let counts = ['a'..'z'] |> Seq.map without
        Seq.min counts
