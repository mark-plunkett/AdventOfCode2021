open System
open System.Collections.Generic

module Part1 =
    
    let is1478 (s : string) =
        match s.Length with
        | 2 | 4 | 3 | 7 -> true
        | _ -> false

    let solve (seq : seq<string>) =
        seq
        |> Seq.collect (fun line -> line.Split("|").[1].Split(" "))
        |> Seq.filter is1478
        |> Seq.length

module Part2 =

    let charSegments = dict [
        (0, 6)
        (1, 2)
        (2, 5)
        (3, 6)
        (4, 4)
        (5, 5)
        (6, 6)
        (7, 3)
        (8, 7)
        (9, 6)
    ]



    let solve (lines : seq<string>) =
        lines
        |> Seq.map (fun line -> line.Split("|").[0])
        |> Seq.filter (fun line ->
            line.Split(" ")
            |> Seq.forall (fun seg -> seg.Length <> 7)
            |> not)
        |> Seq.iter (printfn "%s")

        ""

Seq.initInfinite (fun _ -> Console.ReadLine())
|> Seq.takeWhile (String.IsNullOrEmpty >> not)
|> Part2.solve
|> printfn "%A"
