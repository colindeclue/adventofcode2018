namespace AoC2018
open System


module Day11 =
    let memoize f =
        let cache = ref Map.empty
        fun x ->
        match (!cache).TryFind(x) with
            | Some res -> 
                res
            | None ->
                let res = f x
                cache := (!cache).Add(x,res)
                res

    let hundredsDigit (input:int) =
        let reversed = string input |> Array.ofSeq |> Array.rev |> Seq.map (fun x -> int x - int '0') |> Seq.toArray
        if reversed.Length < 3 then
            0
        else
            reversed.[2]

    let powerLevel (serialNumber:int) (point:int*int) =
        let rackId = fst point + 10
        rackId |> (*) (snd point) |> (+) serialNumber |> (*) rackId |> hundredsDigit |> (+) -5

    let powerLevelCache =
        memoize powerLevel        

    let toSquare (size:int) (point:int*int) =
        seq { for x = fst point to fst point + (size - 1) do
                for y = snd point to snd point + (size - 1) do
                    yield (x, y)
        }

    let power (size:int) (serialNumber:int) (point:int*int) =
        toSquare size point |> Seq.map (powerLevelCache serialNumber) |> Seq.sum

    let powerLevelFromGrid (grid:int[]) (point:int*int) =
        grid.[((fst point - 1) * 300) + (snd point - 1)]

    let powerFromGrid (size:int) (point:int*int) (grid:int[]) =
        toSquare size point |> Seq.map (powerLevelFromGrid grid) |> Seq.sum

    let powerCache =
        memoize power

    let makeGrid (maxSize:int) =
        seq {
            for x = 1 to (300 - maxSize) do
                for y = 1 to (300 - maxSize) do
                    yield (x,y)
        }

    let makeGridWithPower (serialNumber:int) =
        [|for p in makeGrid 0 -> powerLevelCache serialNumber p|]

    let part1 (serialNumber:int) =
        makeGrid 3 |> Seq.maxBy (powerCache 3 serialNumber)

    let getMaxPower (serialNumber:int) (size:int) =
        makeGrid size |> Seq.map (fun x -> (x,(powerCache 3 serialNumber x))) |> Seq.maxBy snd

    let getMaxPowerWithGrid (serialNumber:int) (size:int) =
        let grid = makeGridWithPower serialNumber
        makeGrid size |> Seq.map (fun x -> (x,powerFromGrid size x grid)) |> Seq.maxBy snd

    let getAnswer (serialNumber:int) (max:int) =
        seq {
            for i = 1 to max do
                yield (getMaxPowerWithGrid serialNumber i, i)
        }

    let part2 (serialNumber:int) (max:int) =
        getAnswer serialNumber max |> Seq.maxBy (fun x -> fst x |> snd)
