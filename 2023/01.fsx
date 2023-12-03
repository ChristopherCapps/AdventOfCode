let input = System.IO.File.ReadAllLines "01.input" |> List.ofArray

let lines = input

type DigitAndPosition = {
    digit: int;
    position: int
}

let reverse_string (str: System.String) =
    str
    |> Seq.rev
    |> Array.ofSeq
    |> System.String

let get_first_digit line = 
    line |> (Seq.find System.Char.IsDigit)

let get_first_digit' (line: System.String) =
    let first_digit = get_first_digit line
    (int first_digit - int '0', (System.String line).IndexOf(first_digit))

let get_last_digit line =
    get_first_digit (reverse_string line)

let get_last_digit' line =
    let last_digit = get_last_digit line
    (int last_digit - int '0', (reverse_string line).IndexOf(last_digit))

let get_first_and_last_digits line =
    (get_first_digit line, get_last_digit line)

let get_first_and_last_digits' (line: System.String) =
    let (d, p) = get_first_digit' line
    let first = { digit = d; position = p }
    let (d', p') = get_last_digit' line
    let last = { digit = d'; position = p' }
    first, last

let result = 
    lines 
        |> Seq.map get_first_and_last_digits        
        |> Seq.map (fun (first, last) -> (int first - 48) * 10 + (int last - 48))
        |> Seq.reduce (+)

printfn "Sum: %A" result

let digit_dictionary = Map [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
]

let get_first_spelled_digit (reversed: bool) (line: System.String) =
    let line' = if reversed then (reverse_string line) else line
    let (word, index) = 
        digit_dictionary
        |> Map.keys
        |> Seq.map (fun word -> 
            let word' = if reversed then (reverse_string word) else word
            let idx = line'.IndexOf(word')
            if idx < 0 then (word, System.Int32.MaxValue) else (word, idx))
        |> Seq.minBy snd
    (digit_dictionary[word], index)

let get_last_spelled_digit = get_first_spelled_digit true

let get_first_and_last_spelled_digit' (line: System.String) =
    let (d, p) = get_first_spelled_digit false line
    let first = { digit = d; position = p }
    let (d', p') = get_last_spelled_digit line
    let last = { digit = d'; position = p' }
    first, last

let get_real_first_and_last_digit (line: System.String) =
    let (first_spelled, last_spelled) = get_first_and_last_spelled_digit' line
    let (first, last) = get_first_and_last_digits' line

    let real_first = if first.position < first_spelled.position then first.digit else first_spelled.digit
    let real_last = if last.position < last_spelled.position then last.digit else last_spelled.digit
    real_first, real_last

let result' = 
    lines 
        |> Seq.map get_real_first_and_last_digit        
        |> Seq.map (fun (first: int, last) -> first * 10 + last)
        |> Seq.reduce (+)
