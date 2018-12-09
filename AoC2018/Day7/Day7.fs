module AoC2018
open System.IO
open System.Text.RegularExpressions
open System

module Day7 =

    let (|EmptySeq|_|) a = if Seq.isEmpty a then Some () else None

    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseLine input =
        match input with
        | Regex @"Step ([A-Z]) must be finished before step ([A-Z]) can begin." [prereq;step] -> (step, prereq)
        | _ -> ("","")

    let groupSteps input =
        (fst input, snd input |> Seq.map snd)

    let removeItem (step) (group) =
        (fst group, snd group |> Seq.filter ((<>) step))

    let removeAllItems (steps:seq<string>) (group:string*seq<string>) =
        (fst group, snd group |> Seq.filter (fun x -> not (Seq.contains x steps)))

    let isStartable group =
        (snd group |> Seq.length) = 0

    let isWorkingStepDone (time) (step:string*int) =
        snd step <= time

    let makeWorkingStep (offset) (time) (step:string) =
        (step, time + int step.[0] - int 'A' + 1 + offset)

    let isDone (doneSteps) (group) =
        Seq.contains (fst group) doneSteps

    let notIsDone (doneSteps) (group) =
        (not) (Seq.contains (fst group) doneSteps)

    let getFastForwardGoal (workingSteps) =
        workingSteps |> Seq.minBy snd |> snd

    let isWorkedOn (workingSteps:seq<string*int>) (step:string*seq<string>) =
        workingSteps |> Seq.map fst |> Seq.exists ((=) (fst step))

    let notIsWorkedOn (workingSteps) (step) =
        (not) (isWorkedOn workingSteps step)

    let part1 input =
        let mutable goalWithSteps = readLines input |> Seq.map parseLine |> Seq.groupBy fst |> Seq.map groupSteps
        let allSteps = goalWithSteps |> Seq.map (fun x -> Seq.append (snd x) ([fst x])) |> Seq.concat |> Set.ofSeq
        let mutable startable = goalWithSteps |> Seq.map fst |> Set.ofSeq |> (-) allSteps |> Seq.sort
        startable |> Seq.toList |> printfn "%A"
        let startableGroups = startable |> Seq.map (fun x -> (x,Seq.empty))
        goalWithSteps <- Seq.append goalWithSteps startableGroups
        let mutable stepOrder = Seq.empty
        while Seq.length startable > 0 do
            let step = Seq.head startable
            stepOrder <- Seq.append stepOrder [step]
            let remover = removeItem step
            goalWithSteps <- goalWithSteps |> Seq.filter (fun x -> (fst x) <> step) |> Seq.map remover
            startable <- goalWithSteps |> Seq.filter isStartable |> Seq.map fst |> Seq.sort
            startable |> Seq.toList |> printfn "%A"
            
        stepOrder |> Seq.concat |> Seq.toArray |> String

    let part2 input =
        let workers = 500
        let offset = 60
        let preWork = makeWorkingStep offset
        let mutable goalWithSteps = readLines input |> Seq.map parseLine |> Seq.groupBy fst |> Seq.map groupSteps
        let allSteps = goalWithSteps |> Seq.map (fun x -> Seq.append (snd x) ([fst x])) |> Seq.concat |> Set.ofSeq
        let mutable startable = goalWithSteps |> Seq.map fst |> Set.ofSeq |> (-) allSteps |> Seq.sort |> Seq.toList
        let startableGroups = startable |> Seq.map (fun x -> (x,Seq.empty<string>)) |> Seq.toList
        goalWithSteps <- Seq.append goalWithSteps startableGroups |> Seq.toList
        let mutable workingSteps = Seq.empty<string*int>
        let mutable currentTime = 0
        while (Seq.length startable > 0) || Seq.length workingSteps > 0 do
            let evaluateDone = isWorkingStepDone currentTime
            let startWorking = preWork currentTime
            let doneGroups = workingSteps |> Seq.filter evaluateDone |> Seq.toList
            let doneSteps = doneGroups |> Seq.map fst |> Seq.toList
            // doneSteps |> Seq.toList |> printfn "%A"
            let remover = removeAllItems doneSteps
            let doneCheck = notIsDone doneSteps
            goalWithSteps <- goalWithSteps |> Seq.filter doneCheck |> Seq.map remover |> Seq.toList
            // goalWithSteps |> Seq.length |> printfn "%A"
            // startable |> Seq.length |> printfn "%A"
            let notWorkingOn = notIsWorkedOn workingSteps
            startable <- goalWithSteps |> Seq.filter notWorkingOn |> Seq.filter isStartable |> Seq.map fst |> Seq.sort |> Seq.toList
            workingSteps <- workingSteps |> Seq.except doneGroups |> Seq.toList
            let steps = Seq.take (min (Seq.length startable) (workers - Seq.length workingSteps)) startable |> Seq.toList
            let newWorkingSteps = steps |> Seq.map startWorking |> Seq.toList
            workingSteps <- Seq.append workingSteps newWorkingSteps |> Seq.toList
            currentTime <-
                match workingSteps with
                | EmptySeq -> currentTime
                | x -> getFastForwardGoal x
            currentTime |> printfn "%A"
            workingSteps |> Seq.toList |> printfn "%A"

        currentTime