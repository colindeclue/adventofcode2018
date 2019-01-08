namespace AoC2018
open System.Collections.Generic

module Day24 =
    type Group = {mutable units:int;hp:int;immunities:seq<string>;weaknesses:seq<string>;attack:int;attackType:string;initiative:int}

    let effectiveDamage (attacker:Group) (attackee:Group) =
        let attackType = attacker.attackType
        let mutable modifier = 1
        if Seq.contains attackType attackee.immunities then
            modifier <- 0
        if Seq.contains attackType attackee.weaknesses then
            modifier <- 2
        attacker.attack * modifier * attacker.units

    let dealDamage (attacker:Group) (attackee:Group) =
        if attacker.units > 0 then
            let healthLeft = (attackee.hp * attackee.units) - (effectiveDamage attacker attackee)
            if (healthLeft <= 0) then
                "DEAD" |> printfn "%A"
                attackee.units <- 0
            else
                attackee.units <- (healthLeft - 1) / attackee.hp + 1

    
    let doTargeting (immuneSystem:seq<Group>) (infections:seq<Group>) = 
        let mutable availableInfections = infections |> Seq.filter (fun g -> g.units > 0) |> Seq.toList
        let mutable availableImmune = immuneSystem |> Seq.filter (fun g -> g.units > 0) |> Seq.toList
        let mutable attackGroups = new List<Group*Group>()
        for immune in immuneSystem |> Seq.sortByDescending (fun g -> (g.units * g.attack), g.initiative) do
            let goalTarget = availableInfections |> Seq.sortByDescending (fun g -> (effectiveDamage immune g),(g.units * g.attack), g.initiative) |> Seq.filter (fun g -> (effectiveDamage immune g > 0)) |> Seq.tryHead
            if goalTarget.IsSome then
                attackGroups.Add(immune,goalTarget.Value)
                availableInfections <- availableInfections |> Seq.filter (fun g -> g <> goalTarget.Value) |> Seq.toList
        for infection in infections |> Seq.sortByDescending (fun g -> (g.units * g.attack), g.initiative) do
            let goalTarget = availableImmune |> Seq.sortByDescending (fun g -> (effectiveDamage infection g),(g.units * g.attack), g.initiative) |> Seq.filter (fun g -> (effectiveDamage infection g > 0)) |> Seq.tryHead
            if goalTarget.IsSome then
                attackGroups.Add(infection,goalTarget.Value)
                availableImmune <- availableImmune |> Seq.filter (fun g -> g <> goalTarget.Value) |> Seq.toList
        attackGroups

    let part1 (boost:int) =
        let immuneSystem = [|
                {units=228;hp=8064;immunities=Seq.empty;weaknesses=["cold"];attack=331 + boost;attackType="cold";initiative=8};
                {units=284;hp=5218;immunities=["slashing";"fire"];weaknesses=["radiation"];attack=160 + boost;attackType="radiation";initiative=10};
                {units=351;hp=4273;immunities=["radiation"];weaknesses=Seq.empty;attack=93 + boost;attackType="bludgeoning";initiative=2};
                {units=2693;hp=9419;immunities=["radiation"];weaknesses=["bludgeoning"];attack=30 + boost;attackType="cold";initiative=17};
                {units=3079;hp=4357;immunities=Seq.empty;weaknesses=["radiation";"cold"];attack=13 + boost;attackType="radiation";initiative=1};
                {units=906;hp=12842;immunities=["fire"];weaknesses=Seq.empty;attack=100 + boost;attackType="fire";initiative=6};
                {units=3356;hp=9173;immunities=["fire"];weaknesses=["bludgeoning"];attack=24 + boost;attackType="radiation";initiative=9};
                {units=61;hp=9474;immunities=Seq.empty;weaknesses=Seq.empty;attack=1488 + boost;attackType="bludgeoning";initiative=11};
                {units=1598;hp=10393;immunities=Seq.empty;weaknesses=["fire"];attack=61 + boost;attackType="cold";initiative=20};
                {units=5022;hp=6659;immunities=["bludgeoning";"fire";"cold"];weaknesses=Seq.empty;attack=12 + boost;attackType="radiation";initiative=15}
                |]
        let infection = [|
                {units=120;hp=14560;immunities=["cold"];weaknesses=["radiation";"bludgeoning"];attack=241;attackType="radiation";initiative=18};
                {units=8023;hp=19573;immunities=["bludgeoning";"radiation"];weaknesses=["cold";"slashing"];attack=4;attackType="bludgeoning";initiative=4};
                {units=3259;hp=24366;immunities=["slashing";"radiation";"bludgeoning"];weaknesses=["cold"];attack=13;attackType="slashing";initiative=16};
                {units=4158;hp=13287;immunities=Seq.empty;weaknesses=Seq.empty;attack=6;attackType="fire";initiative=12};
                {units=255;hp=26550;immunities=Seq.empty;weaknesses=Seq.empty;attack=167;attackType="bludgeoning";initiative=5};
                {units=5559;hp=21287;immunities=Seq.empty;weaknesses=Seq.empty;attack=5;attackType="slashing";initiative=13};
                {units=2868;hp=69207;immunities=["fire"];weaknesses=["bludgeoning"];attack=33;attackType="cold";initiative=14};
                {units=232;hp=41823;immunities=["bludgeoning"];weaknesses=Seq.empty;attack=359;attackType="bludgeoning";initiative=3};
                {units=729;hp=41762;immunities=Seq.empty;weaknesses=["fire";"bludgeoning"];attack=109;attackType="fire";initiative=7};
                {units=3690;hp=36699;immunities=Seq.empty;weaknesses=Seq.empty;attack=17;attackType="slashing";initiative=19}
                |]
        let mutable immuneUnits = immuneSystem |> Seq.sumBy (fun g -> g.units)
        let mutable infectionUnits = infection |> Seq.sumBy (fun g -> g.units)
        while immuneUnits > 0 && infectionUnits > 0 do
            let targets = doTargeting immuneSystem infection
            for target in targets |> Seq.sortByDescending (fun a -> (fst a).initiative) do
                dealDamage (fst target) (snd target)
            immuneUnits <- immuneSystem |> Seq.sumBy (fun g -> g.units)
            infectionUnits <- infection |> Seq.sumBy (fun g -> g.units)
        immuneUnits|> printfn "%A"
        infectionUnits |> printfn "%A"
        