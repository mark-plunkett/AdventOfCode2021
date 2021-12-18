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

module Part1 =

    let closeScores = dict [
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
        |> Option.map (fun c -> closeScores.[c])

    let solve input =
        input
        |> Seq.map scoreLine
        |> Seq.choose id
        |> Seq.sum

module Part2 =

    let closeScores = dict [
        ')', 1L
        ']', 2L
        '}', 3L
        '>', 4L
    ]

    let scoreLine (line : string) =
        line
        |> Seq.fold (fun closeChars openChar ->
            match pairs.TryGetValue openChar with
            | true, close -> close :: closeChars
            | false, _ -> List.tail closeChars
        ) []
        |> List.fold (fun total c -> (5L * total) + closeScores.[c]) 0L

    let solve input =
        input
        |> Seq.filter (Part1.scoreLine >> Option.isNone)
        |> Seq.map scoreLine
        |> Seq.sort
        |> Seq.toArray
        |> fun a -> a.[a.Length / 2]

Seq.initInfinite (fun _ -> Console.ReadLine())
|> Seq.takeWhile (String.IsNullOrEmpty >> not)
|> Part2.solve
|> printfn "%A"