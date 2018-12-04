namespace AoC2018
open System.IO
open System.Text.RegularExpressions
open System

module Day4 =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let GetDate (input) = 
        match input with
        | Regex @"\[([^\]]+)]" [date] -> (DateTime.Parse date).AddHours(1.0)
        | _ -> DateTime.MinValue
    
    let GetLine (input) = 
        match input with
        | Regex @"\[([^\]]+)](.+)" [date;rest] -> 
            (DateTime.Parse date).AddHours 1.0, rest
        | _ -> DateTime.MinValue, ""

    let GetGuardId (input) =
        match input with
        | Regex @"#(\d+)" [id] -> id
        | _ -> ""

    type DayCollection = DateTime * seq<DateTime * string>
    let GetGuardAndAsleepMinutes (day:DayCollection) =
        let guardId =  snd day |> Seq.head |> snd |> GetGuardId
        let mutable asleepMinutes = Seq.empty
        let mutable rangeStart = 0
        for changes in (snd day) |> Seq.skip 1 do
            if snd changes = " falls asleep" then
                rangeStart <- (fst changes).Minute
            else
                asleepMinutes <- Seq.append [rangeStart..(fst changes).Minute - 1] asleepMinutes
                rangeStart <- 0
        
        (guardId, asleepMinutes)

    let getDate (input:DateTime) =
        input.Date

    let countItem (seq) (x) =  seq |> Seq.filter ((=) x) |> Seq.length

    let combine (inputs) =
        (fst inputs), snd inputs |> Seq.map snd |> Seq.concat
    let part1 (input) =
        let days = input |> readLines |> Seq.map GetLine |> Seq.sortBy fst |> Seq.groupBy (fun x -> fst x |> getDate)
        let answer = days |> Seq.map GetGuardAndAsleepMinutes |> Seq.groupBy fst |> Seq.map combine |> Seq.sortByDescending (fun x -> snd x |> Seq.length) |> Seq.head
        (fst answer |> int) * (snd answer |> Seq.maxBy (fun x -> countItem (snd answer) x))

    let part2 (input) =
        let days = input |> readLines |> Seq.map GetLine |> Seq.sortBy fst |> Seq.groupBy (fun x -> fst x |> getDate)
        let answer = days |> Seq.map GetGuardAndAsleepMinutes |> Seq.groupBy fst |> Seq.map combine |> Seq.filter (fun x -> (snd x |> Seq.length) > 0) |> Seq.sortByDescending (fun x -> snd x |> Seq.maxBy (fun y -> countItem (snd x) y)) |> Seq.head
        (fst answer |> int), (snd answer |> Seq.maxBy (fun x -> countItem (snd answer) x))