let input = System.IO.File.ReadAllLines "03.input" |> List.ofArray

let priority item =
    match item with
    | _ when item >= 'a' && item <= 'z' -> (int item) - (int 'a') + 1
    | _ when item >= 'A' && item <= 'Z' -> (int item) - (int 'A') + 27
    | _ -> failwithf "Unrecognized item type: %A" item

type Rucksack = {
    first: string
    second: string
}

let rucksackOf line =
    let len = String.length line
    { first = line.[0..len/2-1]; second = line.[len/2..] }

let rucksacks =
    input
    |> List.map rucksackOf

let sharedItem rucksack =
    rucksack.first
    |> Seq.filter (fun item -> rucksack.second.Contains item)
    |> Seq.head

let sharedItems =
    rucksacks
    |> List.map sharedItem

let sharedItemPrioritiesTotal =
    sharedItems
    |> List.map priority
    |> List.sum

printfn "%A" sharedItemPrioritiesTotal

// Part 2

let elfGroups =
    rucksacks
    |> List.chunkBySize 3

let isCommonItem group (item: char) =
    group
    |> List.filter (fun sack -> (sack.first.Contains item) || (sack.second.Contains item))
    |> List.length
    |> (=) 3

let getCommonItem (group: Rucksack list) =
    group
    |> List.head
    |> fun sack -> sack.first + sack.second
    |> Seq.distinct
    |> Seq.filter (isCommonItem group)
    |> Seq.head

let groupCommonItems =
    elfGroups
    |> List.map getCommonItem

let groupCommonItemPriorities =
    groupCommonItems
    |> List.map priority
    |> List.sum

printfn "Common item priority sum: %A" groupCommonItemPriorities