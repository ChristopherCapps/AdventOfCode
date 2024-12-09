let input = System.IO.File.ReadAllLines "2024/07.input"

let lines = input

type Expression = { result: int; operands: int array }

let parse_expression (line: string) =
    let idx = line.IndexOf(':')
    let result' = int (line.Substring(0, idx))

    let operands' =
        line
            .Substring(idx + 1)
            .Trim()
            .Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int

    { result = result'
      operands = operands' }

let operators = [ (+); (*) ]
