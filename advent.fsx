open System

let day1test =
    """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""


printfn "Day 1"
let day1input = System.IO.File.ReadAllText "day1.txt"

let find_max (x: string, y: int) : int =
    x.Split($"{Environment.NewLine}{Environment.NewLine}", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x.Split($"{Environment.NewLine}", StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun x -> x |> Array.map int |> Array.sum)
    |> Array.sortDescending
    |> Array.take (y)
    |> Array.sum


printfn "Test input: %A" (find_max (day1test, 1))
printfn "Day1 Part 1 Input: %A" (find_max (day1input, 1))
printfn "Day1 Part 2 Input: %A" (find_max (day1input, 3))

printfn "Day 2"

let day2test =
    """
A Y
B X
C Z
"""

let win = 6
let draw = 3
let lose = 0

let rock = 1
let paper = 2
let scissors = 3

let build_struct (x: string) =
    x.Split($"{Environment.NewLine}", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x.Trim())
    |> Array.map (fun x -> x.Split(" ", StringSplitOptions.RemoveEmptyEntries))

// part 1

let score_rock (x: string) =
    match x with
    | "X" -> rock + draw
    | "Y" -> paper + win
    | "Z" -> scissors + lose
    | _ -> raise (ArgumentException("Invalid move"))

let score_paper (x: string) =
    match x with
    | "X" -> rock + lose
    | "Y" -> paper + draw
    | "Z" -> scissors + win
    | _ -> raise (ArgumentException("Invalid move"))

let score_scissors (x: string) =
    match x with
    | "X" -> rock + win
    | "Y" -> paper + lose
    | "Z" -> scissors + draw
    | _ -> raise (ArgumentException("Invalid move"))

let part1_score (x: string, y: string) =
    match x with
    | "A" -> score_rock y
    | "B" -> score_paper y
    | "C" -> score_scissors y
    | _ -> raise (ArgumentException("Invalid move"))

let build_game1 (x: string) =
    build_struct x |> Array.map (fun x -> part1_score (x[0], x[1])) |> Array.sum

// part 2

let score_win (x: string) =
    match x with
    | "A" -> paper
    | "B" -> scissors
    | "C" -> rock
    | _ -> raise (ArgumentException("Invalid move"))

let score_draw (x: string) =
    match x with
    | "A" -> rock
    | "B" -> paper
    | "C" -> scissors
    | _ -> raise (ArgumentException("Invalid move"))

let score_lose (x: string) =
    match x with
    | "A" -> scissors
    | "B" -> rock
    | "C" -> paper
    | _ -> raise (ArgumentException("Invalid move"))

let part2_score (x: string, y: string) =
    match y with
    | "X" -> lose + score_lose x
    | "Y" -> draw + score_draw x
    | "Z" -> win + score_win x
    | _ -> raise (ArgumentException("Invalid move"))

let build_game2 (x: string) =
    build_struct x |> Array.map (fun x -> part2_score (x[0], x[1])) |> Array.sum

let day2input = System.IO.File.ReadAllText "day2.txt"

// print the contents of foo
printfn "Day2 Test: %A" (build_game1 day2test)
printfn "Day2 Part 1: %A" (build_game1 day2input)
printfn "Day2 Part 2: %A" (build_game2 day2input)
