let input = System.IO.File.ReadAllLines "03.input" 

let lines = input

let parse_digits (s: string) =
    let rec parse_digits_rec (s': char list) (acc: char list) =
        //printfn "Input: %O, Acc: %O" s' acc
        match s' with
        | head::tail -> if (head >= '0' && head <= '9') then parse_digits_rec tail (acc @ [head]) else acc
        | [] -> acc
    let result = parse_digits_rec (List.ofSeq s) []
    //printfn "Result: %O" result
    match result with
    | [] -> None
    | _ as digits -> (string digits) |> int |> Some

