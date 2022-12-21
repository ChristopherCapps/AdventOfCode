let input = System.IO.File.ReadAllLines "01.input" |> List.ofArray

let add_calories elf_calories calories =
    if (String.length calories <> 0) then
        //printfn "%A" calories
        List.insertAt 0 ((int calories)::(List.head elf_calories)) (List.tail elf_calories)
    else         
        List.insertAt 0 [] elf_calories

let elves =
    input
    |> List.fold add_calories [[]]

let total_elf_calories =
    elves 
    |> List.map List.sum

let max_elf_calories =
    total_elf_calories
    |> List.max

printfn "Maximum elf calories: %A" max_elf_calories

// Part 2

let top_three_calories_total =
    total_elf_calories
    |> List.sort
    |> List.rev
    |> List.take 3
    |> List.sum

printfn "Top 3 total elf calories: %A" top_three_calories_total