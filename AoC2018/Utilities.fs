namespace Aoc2018

module Utilities =
    let stringToIntList (input: string) = input |> Seq.map (string >> int) |> Seq.toList
    let stringToIntSeq  (input: string) = input |> Seq.map (string >> int)