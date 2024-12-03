let input = System.IO.File.ReadAllLines "01.input" |> List.ofArray

let lines = input

let raw_locations = 
    lines 
    |> Seq.map (fun (l: System.String) -> l.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries));

let (locations_a, locations_b) = 
    raw_locations
    |> Seq.fold 
        (fun (location_lists: int list * int list as (a,b)) (locations: string array) -> ([(int (locations.[0].Trim()))] @ a, [(int (locations.[1].Trim()))] @ b))
        ([], [])

let locations_a' = List.sort locations_a
let locations_b' = List.sort locations_b

let diffs = 
    Seq.map2 (-) locations_a' locations_b'
    |> Seq.map abs

let sum = Seq.sum diffs

let frequencies_b =
    let update_map (f_map: Map<int, int>) value =
        Map.change value (fun current_tally -> 
            match current_tally with
            | Some tally -> Some (tally + 1)
            | None -> Some 1)
            f_map
    locations_b' 
    |> Seq.fold update_map Map.empty

let similarity_score =
    locations_a'
    |> Seq.fold (fun score location -> 
        match (Map.tryFind location frequencies_b) with
        | Some frequency -> score + (frequency * location)
        | None -> score)
        