namespace AoC2018
open System.Collections.Generic

module Day14 =

    let getNewScores (score:int) =
        if score > 9 then
            [|score / 10;score % 10|]
        else
            [|score|]

    let part1 (count:int) =
        let mutable recipes = new List<int>([|3;7|])
        let mutable elfs = (0,1)
        for i = 1 to count + 10 do
            let score1 = recipes.[fst elfs]
            let score2 = recipes.[snd elfs]
            recipes.AddRange(getNewScores (score1 + score2))
            let recipeLength = recipes.Count
            elfs <- ((score1 + 1 + fst elfs) % recipeLength, (score2 + 1 + snd elfs) % recipeLength)
            if i % 1000 = 0 then
                i |> printfn "%A"
        // recipes
        recipes.GetRange(count,10) |> Seq.toList

    let part2 (goal:int[]) =
        let mutable recipes = new List<int>([|3;7|])
        let mutable elfs = (0,1)
        let mutable found = false
        let mutable offset = -1
        while not found do
            let score1 = recipes.[fst elfs]
            let score2 = recipes.[snd elfs]
            recipes.AddRange(getNewScores (score1 + score2))
            let recipeLength = recipes.Count
            elfs <- ((score1 + 1 + fst elfs) % recipeLength, (score2 + 1 + snd elfs) % recipeLength)
            if recipeLength > goal.Length + 1 then
                let check = recipes.GetRange(recipeLength-goal.Length-1,goal.Length) |> Seq.toArray
                found <- check = goal
                if not found then
                    let otherCheck = recipes.GetRange(recipeLength-goal.Length-2,goal.Length) |> Seq.toArray
                    found <- otherCheck = goal
                    offset <- -2
            if recipeLength % 10000 = 0 then
                recipeLength |> printfn "%A"
        recipes.GetRange(9,5) |> Seq.toList |> printfn "%A"
        recipes.Count - goal.Length + offset
