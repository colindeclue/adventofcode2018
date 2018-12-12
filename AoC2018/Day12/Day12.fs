namespace AoC2018
open System.IO
open System.Text.RegularExpressions

module Day12 =

    let memoize f =
        let cache = ref Map.empty
        fun x ->
        match (!cache).TryFind(x) with
            | Some res -> 
                "From cache" |> printf "%A"
                res
            | None ->
                let res = f x
                cache := (!cache).Add(x,res)
                res

    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseStartState input =
        match input with
        | Regex @"initial state: (.*)" [s] -> s |> Seq.mapi (fun (i) (x) -> (i,x='#')) |> Seq.toArray
        | _ -> [||]

    let parseRule input =
        match input with
        | Regex @"(.)(.)(.)(.)(.) => (.)" [first;second;third;fourth;fith;res] -> (res = "#",[|first="#";second="#";third="#";fourth="#";fith="#"|])
        | _ -> (false,[||])

    let getNextState (rules:seq<bool*bool[]>) (group:bool[]) =
        match rules |> Seq.tryFind (fun x -> snd x = group) with
            | Some x -> fst x
            | None -> false

    let getNextArray (rules:seq<bool*bool[]>) (plants:(int*bool)[]) =
        let firstIndex = fst plants.[0]
        let lastIndex = plants |> Array.last |> fst
        let arr' = Array.append plants [|(lastIndex+1,false);(lastIndex+2,false);(lastIndex+3,false)|] |> 
                    Array.append [|(firstIndex-3,false);(firstIndex-2,false);(firstIndex-1,false)|] |>
                    Array.windowed 5 |> 
                    Array.map (fun x -> 
                        (fst x.[2],(getNextState rules (x |> Array.map snd))))

        let startIndex = arr' |> Array.findIndex (fun x -> snd x)
        let endIndex = arr' |> Array.findIndexBack (fun x -> snd x)
        arr'.[startIndex..endIndex]
        

    let part1 input =
        let lines = readLines input
        let mutable plants = lines |> Seq.head |> parseStartState
        let rules = lines |> Seq.tail |> Seq.map parseRule
        for i = 1 to 1000 do 
            plants <- getNextArray rules plants
            plants |> Seq.filter (fun x -> snd x) |> Seq.map fst |> Seq.sum |> printfn "%A"
        plants |> Seq.filter (fun x -> snd x) |> Seq.map fst |> Seq.sum