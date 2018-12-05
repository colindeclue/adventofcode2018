namespace AoC2018
open System
open System.IO
open System.Collections

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

    let reducer (stack:ImmutableStack<char>) (char) =
        if stack.IEmpty then
            stack.Push(char)
        else
            let top = stack.Peek()
            if not (compareChars [|top;char|]) then
                stack.Push(char)
            else
                stack.Top()

    let all (input:ImmutableStack<char>) =
        input.All()

    let fullReduceFaster word =
        Seq.fold reducer ImmutableStack.Empty word |> all |> List.length

    let part1Fast (input) =
        readLine input |> fullReduceFaster

    let withoutChar (input:string) (c:char) =
        input.Replace(c,' ').Replace(c |> Char.ToUpper,' ').Replace(" ","") |> fullReduceFaster

    let part2 (input) =
        let polymer = readLine input
        let without = withoutChar polymer
        let counts = ['a'..'z'] |> Seq.map without
        Seq.min counts
