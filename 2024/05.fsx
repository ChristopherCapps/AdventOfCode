let input = System.IO.File.ReadAllLines "05.input"

let lines = input

type PageOrderRule = PageOrderRule of before: int * after: int

type PageUpdate = PageUpdate of int list

let (|OrderRule|_|) (line: string) =
    let idx = line.IndexOf('|')

    if (idx > -1) then
        Some(PageOrderRule(int (line.Substring(0, idx)), int (line.Substring(idx + 1))))
    else
        None

let (|Update|_|) (line: string) =
    let idx = line.IndexOf(',')

    if (idx > -1) then
        let pages = 
            line.Split([|','|])
            |> Array.map int
            |> List.ofArray
        Some(PageUpdate(pages))
    else
        None

let rules, updates =
    lines
    |> Seq.fold (fun (rules, updates) line ->
        match line with 
        | OrderRule rule -> (rules @ [rule], updates)
        | Update pages -> (rules, updates @ [pages])
        | _ -> (rules, updates))
        ([], [])

let obeys_rules (rules: PageOrderRule list) (pages: int list) =
    rules
    |> List.forall (fun (PageOrderRule(before, after)) ->
        let beforeIndex = pages |> List.tryFindIndex ((=) before)
        let afterIndex = pages |> List.tryFindIndex ((=) after)
        
        match beforeIndex, afterIndex with
        | Some bi, Some ai -> bi < ai  // before page should appear earlier in list
        | None, _ | _, None -> true    // if either page isn't in the list, rule doesn't apply
    )

let valid_updates =
    updates
    |> List.filter (fun (PageUpdate pages) -> obeys_rules rules pages)

let result =
    valid_updates
    |> List.map (fun (PageUpdate pages) -> 
        let mid = pages.Length / 2
        pages.[mid])
    |> List.sum

printfn "Result: %d" result

let reorder_pages (rules: PageOrderRule list) (pages: int list) =
    let rec try_moves pages =
        let violations =
            rules
            |> List.collect (fun (PageOrderRule(before, after)) ->
                let beforeIndex = pages |> List.findIndex ((=) before)
                let afterIndex = pages |> List.findIndex ((=) after)
                if beforeIndex < afterIndex then [] 
                else [(beforeIndex, afterIndex)]
            )
            
        match violations with
        | [] -> pages // No violations, pages are in correct order
        | (wrongIdx, correctIdx)::_ ->
            // Move the violating page to correct position
            let pageToMove = pages.[wrongIdx]
            let newPages = 
                pages 
                |> List.removeAt wrongIdx
                |> List.insertAt correctIdx pageToMove
            try_moves newPages // Recursively fix remaining violations
            
    try_moves pages

let invalid_updates =
    updates
    |> List.filter (fun (PageUpdate pages) -> not (obeys_rules rules pages))
    
let fixed_updates =
    invalid_updates
    |> List.map (fun (PageUpdate pages) -> 
        PageUpdate(reorder_pages rules pages))

let result' =
    fixed_updates
    |> List.map (fun (PageUpdate pages) -> 
        let mid = pages.Length / 2
        pages.[mid])
    |> List.sum

printfn "Result': %d" result'