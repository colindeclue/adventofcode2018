namespace AoC2018

module Day20171 =
    let matchesNext (x: int) (i: int) (list: List<int>) = 
        list.[(i + 1) % list.Length] = x

    let matchesHalfWayRound (x:int) (i:int) (list:List<int>) =
        list.[(i + list.Length / 2) % list.Length] = x
    
    let filterWithIndex (list: List<int>) =
        seq{ 
            for i = 0 to Seq.length list - 1 do
                if matchesHalfWayRound list.[i] i list then
                    yield list.[i]
        }

    let stringToList (input: string) = input |> Seq.map (string >> int) |> Seq.toList
    