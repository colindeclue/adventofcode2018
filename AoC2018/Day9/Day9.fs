namespace AoC2018
open System.Collections.Generic
open System.Diagnostics

module Day9 =
    type Node = | NodeContents of (int*Node*Node)

    let moveClockwise (count:int) (currentMarble:LinkedListNode<uint64>) =
        let mutable current = currentMarble
        for i = 1 to count do
            if current.Next = null then
                current <- current.List.First
            else
                current <- current.Next
        current

    let moveCounterClockwise (count:int) (currentMarble:LinkedListNode<uint64>) =
        let mutable current = currentMarble
        for i = 1 to count do
            if current.Previous = null then
                current <- current.List.Last
            else
                current <- current.Previous
        current

    let addMarblePart2 (currentMarble:LinkedListNode<uint64>) (marble:uint64) (circle:LinkedList<uint64>) =
        if marble % 23UL = 0UL then
            let other = moveCounterClockwise 7 currentMarble
            let score = other.Value + marble
            let newCurrent = moveClockwise 1 other
            circle.Remove other
            (newCurrent, score)
        else
            let mutable insertAfter = currentMarble.Next
            if insertAfter = null then
                insertAfter <- circle.First
            (circle.AddAfter (insertAfter, marble), 0UL)

    
    let addMarble (currentIndex:int) (marble:int) (arr:ResizeArray<int>) =
        if marble % 23 = 0 then
            let mutable other = currentIndex - 7
            if other < 0 then
                other <- other + arr.Count
            other |> printfn "%A"
            let score = marble + arr.[other]
            score |> printfn "%A"
            arr.RemoveAt(other)
            (other % arr.Count, score)
        else
            let newIndex = (currentIndex + 2) % arr.Count
            arr.Insert(newIndex, marble)
            (newIndex,0)

    let part1 (numPlayers:int) marbleCount =
        let players = Array.init numPlayers (fun x -> 0UL)
        let mutable currentPlayer = 1
        let circle = new LinkedList<uint64>()
        let mutable current = circle.AddFirst 0UL
        for marble = 1 to marbleCount do
            let playerIndex = currentPlayer % numPlayers
            let (newItem, score) = addMarblePart2 current (uint64 marble) circle
            players.[playerIndex] <- players.[playerIndex] + score
            current <- newItem
            currentPlayer <- currentPlayer + 1
        players |> Seq.max
        
    let part2 numPlayers marbleCount =
        let sw = Stopwatch.StartNew()
        part1 numPlayers (marbleCount * 100) |> printfn "%A"
        sw.Stop()
        sw.ElapsedMilliseconds

    let measure f =
        let sw = new Stopwatch();
        sw.Start()
        let answer = f
        sw.Stop()