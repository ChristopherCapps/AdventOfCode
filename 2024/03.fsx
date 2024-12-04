let input = System.IO.File.ReadAllLines "2024/03.input" 

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
    | _ as digits -> (System.String (Array.ofList digits)) |> Some

let consume_mul (s: string) =
    s.Length >= 4 && s.Substring(0, 4).Equals("mul(")

let consume_comma (s: string) =
    s.Length >= 1 && s.[0] = ','

let consume_close_parenthesis (s:string) =
    s.Length >= 1 && s.[0] = ')'

let consume_do (s:string) =
    s.Length >= 4 && s.StartsWith("do()")

let consume_dont (s:string) =
    s.Length >= 7 && s.StartsWith("don't()")

let consume_line (s: string) (start_enabled: bool)=
    let mutable s' = s;
    let mutable products = []    
    let mutable enabled = start_enabled
    while (s'.Length >= 8) do
        if s' |> consume_mul then
            s' <- s'.Substring(4)
            match s' |> consume_digits with
            | Some digits -> 
                s' <- s'.Substring(digits.Length)
                if s' |> consume_comma then
                    s' <- s'.Substring(1)
                    match s' |> consume_digits with
                    | Some digits' -> 
                        s' <- s'.Substring(digits'.Length)
                        if s' |> consume_close_parenthesis then
                            s' <- s'.Substring(1) // advance 1 char
                            if enabled then products <- products @ [(int digits) * (int digits')]
                        else s' <- s'.Substring(1) // Couldn't consume ")", so move to next char
                    | None -> s' <- s'.Substring(1) // Couldn't consume a digit, so move to next char
                else s' <- s'.Substring(1) // Couldn't consume ",", so move to next char
            | None -> s' <- s'.Substring(1) // Couldn't consume a digit, so move to next char
        else 
            if s' |> consume_do then 
                enabled <- true
                s' <- s'.Substring(4) // Consume do()
            else if s' |> consume_dont then
                enabled <- false
                s' <- s'.Substring(7) // Consume don't()
            else 
                s' <- s'.Substring(1) // Couldn't consume mul, do, don't so advance 1
    products, enabled

let mutable enabled = true
let mutable products = []

for line in lines do
    let products', enabled' = consume_line line enabled
    enabled <- enabled'
    products <- products @ products'

products |> List.reduce (+)

// lines 
// |> Seq.map consume_line
// |> Seq.map (List.reduce (+))
// |> Seq.reduce (+)