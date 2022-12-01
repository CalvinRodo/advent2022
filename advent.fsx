open System

let day1test = """
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

let find_max  (x: string, y: int) : int = x.Split($"{Environment.NewLine}{Environment.NewLine}", StringSplitOptions.RemoveEmptyEntries)
                                          |> Array.map (fun x -> x.Split($"{Environment.NewLine}", StringSplitOptions.RemoveEmptyEntries))  
                                          |> Array.map (fun x -> x |> Array.map int |> Array.sum)
                                          |> Array.sortDescending
                                          |> Array.take (y)
                                          |> Array.sum


printfn "Test input: %A" (find_max (day1test, 1))
printfn "Day1 Part 1 Input: %A" (find_max (day1input, 1))
printfn "Day1 Part 2 Input: %A" (find_max (day1input, 3))
