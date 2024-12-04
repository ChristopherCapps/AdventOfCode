let input = System.IO.File.ReadAllLines "03.input" 

let lines = input

let consume_digits (s: string) =
    let rec parse_digits_rec (s': char list) (acc: char list) =
        //printfn "Input: %O, Acc: %O" s' acc
        match s' with
        | head::tail -> if (head >= '0' && head <= '9') then parse_digits_rec tail (acc @ [head]) else acc
        | [] -> acc
    let result = parse_digits_rec (List.ofSeq s) []
    //printfn "Result: %O" result
    match result with
    | [] -> None
    | _ as digits -> (System.String (Array.ofList digits)) |> int |> Some

let consume_mul (s: string) =
    s.Length >= 4 && s.Substring(0, 4).Equals("mul(")

let consume_comma (s: string) =
    s.Length >= 1 && s.[0] = ','

let consume_close_parenthesis (s:string) =
    s.Length >= 1 && s.[0] = ')'

