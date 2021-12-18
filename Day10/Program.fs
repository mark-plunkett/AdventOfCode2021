open System

let tee f o =
    f o
    o

let pairs = dict [
    '[', ']'
    '(', ')'
    '{', '}'
    '<', '>'
]

let points = dict [
    ')', 3
    ']', 57
    '}', 1197
    '>', 25137
]

let scoreLine (line : string) =
    line
    |> Seq.scan (fun (closes, _) c ->
        match pairs.TryGetValue c with
        | true, close -> close :: closes, None
        | false, _ -> 
            // its a closing char
            if List.head closes = c then List.tail closes, None
            else closes, Some c
    ) ([], None)
    |> Seq.tryPick snd
    |> Option.map (fun c -> points.[c])

Seq.initInfinite (fun _ -> Console.ReadLine())
|> Seq.takeWhile (String.IsNullOrEmpty >> not)
|> Seq.map scoreLine
|> Seq.choose id
|> Seq.sum
|> Console.WriteLine