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


printfn "Day 3"

let day3test = """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""

let getScore (x: char) = 
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".IndexOf(x) + 1


let splitStringInHalf(x: string) =
    let half = (x.Length / 2)
    (x.Substring(0, half).ToCharArray() |> Array.toList), (x.Substring(half, half).ToCharArray() |> Array.toList)

let intersection (x) =

    let (a, b) = x
    a |> List.filter (fun x -> b |> List.contains x) 

let getListOfStrings(x : string) = 
    x.Split($"{Environment.NewLine}", StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let calcPrioritiesPart1 (x: string) = 
    x |> getListOfStrings
    |> List.map (fun x -> splitStringInHalf x)
    |> List.map (fun x -> intersection x)
    |> List.map (fun x -> List.distinct x)
    |> List.map (fun x -> 
        x 
        |> List.map getScore 
        |> List.sum)
    |> List.sum

let getCommon(x: List<List<char>>) =
    let fx = intersection (x.Item 1, x.Item 0) 
    intersection (fx, x.Item 2)

let calcPrioritiesPart2 (x: string) = 
    x 
    |> getListOfStrings
    |> List.map (fun x -> x.ToCharArray() |> Array.toList)
    |> List.chunkBySize 3
    |> List.map (fun x -> getCommon x)
    |> List.map(fun x -> List.distinct x)
    |> List.map (fun x -> 
        x 
        |> List.map getScore 
        |> List.sum)
    |> List.sum


let day3input = System.IO.File.ReadAllText "day3.txt"

printfn "Day3 Test: %A" (calcPrioritiesPart1 day3test) 
printfn "Day3 Part1: %A" (calcPrioritiesPart1 day3input)
printfn "Day3 Part2: %A" (calcPrioritiesPart2 day3input)

printfn "Day 4"

let day4test = """
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"""

let getArray (start: string, finish: string) =
    Array.init (int finish - int start + 1) (fun x -> x + int start)

let ifContains (x , y) = 
    let intersect = intersection ((x, y))
    x = intersect || y = intersect

let ifOverlaps (x, y) =
    let intersect = intersection ((x, y))
    intersect.Length > 0

let day4 (x: string, check) =
    x.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x.Split(",", StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun x -> x |> Array.map (fun y -> y.Split("-", StringSplitOptions.RemoveEmptyEntries)))
    |> Array.map (fun x -> x |> Array.map (fun y -> getArray (y[0], y[1])))
    |> Array.filter (fun x -> check (x[0] |> Array.toList, x[1] |> Array.toList))
    |> Array.length


printfn "day4test %A" (day4  (day4test, ifContains))
let day4input = System.IO.File.ReadAllText "day4.txt"
printfn "day4inputPart1 %A" (day4 (day4input,ifContains))
printfn "day4inputPart2 %A" (day4 (day4input,ifOverlaps))

printfn "Day 5"

let day5teststack = """
ZN
MCD
P
"""

let day5testmoves = """
move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""

let build_stacks (x: string) =
    x.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x.ToCharArray() |> Array.toList)

let parse_moves (x: string) = 
    x.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x.Split(" ", StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun x -> (int x[1], int x[3], int x[5]))
    |> Array.toList

let move (stacks: List<char>[], move: int * int * int, move_multiple: bool) =
    let (num, move_from, move_to) = move
    let stackToMove = stacks[move_to - 1]
    let stackToMoveFrom = stacks[move_from - 1]
    let itemsToTake = stackToMoveFrom |> List.rev |> List.take num 
    stacks.[move_to - 1] <- stackToMove @ if move_multiple then (itemsToTake |> List.rev) else itemsToTake
    stacks.[move_from - 1] <- stackToMoveFrom |> List.rev |> List.skip num |> List.rev

let move_stacks (stacks: List<char>[], moves: List<int * int * int>, move_multiple: bool) =
    for i in moves do
        move (stacks, i, move_multiple)
    stacks

let day5StacksInput = System.IO.File.ReadAllText "day5stacks.txt"
let day5MovesInput = System.IO.File.ReadAllText "day5moves.txt"
printfn "%A" (move_stacks (day5StacksInput |> build_stacks, day5MovesInput |> parse_moves, false) |> Array.map (fun x -> x |> List.last |> string) |> String.concat "")
printfn "%A" (move_stacks (day5StacksInput |> build_stacks, day5MovesInput |> parse_moves, true) |> Array.map (fun x -> x |> List.last |> string)  |> String.concat "")

printfn "Day 6"

let findStartOfPacket (x: string, packetLength: int) = 
    let rec findStartOfPacket (x: string, packetLength: int, index: int) : int =
        if x[index - packetLength .. index - 1].ToCharArray() 
                |> Array.distinct 
                |> Array.length = packetLength then 
            index
        else 
            findStartOfPacket(x, packetLength, index + 1)
    findStartOfPacket(x, packetLength, packetLength + 1)


let day6Test =  [
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb"; // 7
    "bvwbjplbgvbhsrlpgdmjqwftvncz"; // 5
    "nppdvjthqldpwncqszvftbrmjlhg"; //6
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"; // 10
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" // 11
]

for i in day6Test do 
    printfn "Day6Test = %A" (findStartOfPacket (i, 4))
    printfn "Day6Test = %A" (findStartOfPacket (i, 14))

let day6input : string = System.IO.File.ReadAllText "day6.txt"

printfn "Day6Part1 = %A" (findStartOfPacket (day6input, 4))
printfn "Day6Part1 = %A" (findStartOfPacket (day6input, 14))

printfn "Day 7"

let day7testinput = """
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""

type file = {
    name: string
    size: int
}

type dir =
    {
        name : string
        mutable size: int
        mutable children: List<dir>
        mutable files: List<file>
        parent : Option<dir>
    }

let mutable root: dir = {name = "root"; size = 0; files = List.empty; children = List.empty; parent = None }
let mutable curDir = root

let buildCmdTree(input: string) =
    input.Split("$", StringSplitOptions.RemoveEmptyEntries )
    |> Array.map(fun x -> x.Trim())
    |> Array.map(fun x -> x.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries))
    |> Array.filter(fun x -> x.Length <> 0)

let setCurDir(dir: string) =
    match dir with
    | "/" -> curDir <- root
    | ".." -> curDir <- curDir.parent.Value
    | _ -> curDir <- (curDir.children |> List.find( fun x -> x.name = dir))

let buildFS(cmd: string[]) =
    for output in cmd[1..] do
        let sOutput = output.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        if sOutput[0].StartsWith("dir") then
            curDir.children <- { name = sOutput[1]; size = 0; files = List.empty; children = List.empty; parent = Some(curDir) } :: curDir.children
        else
            curDir.files <- { name = sOutput[1]; size = int sOutput[0]} :: curDir.files

let processCmds(cmds: string[][]) =
    for cmd in cmds do
        if cmd[0].StartsWith("cd") then
            setCurDir(cmd[0].Split(" ", StringSplitOptions.RemoveEmptyEntries)[1])
        else
            buildFS(cmd)

let getAllDirectorySizes(root: dir) =
    let mutable dir_sizes: List<int> = List.Empty
    let rec getDir(currentDir: dir) =
        let sumOfChildren =
            currentDir.children
            |> List.map getDir
            |> List.sum

        let sumOfFiles =
            currentDir.files
            |> List.map (fun f -> f.size)
            |> List.sum
        
        currentDir.size <- sumOfChildren + sumOfFiles
        dir_sizes <- currentDir.size :: dir_sizes
        currentDir.size
        // Get the total of all sizes
    root.size <- getDir(root)
    dir_sizes <- root.size :: dir_sizes
    dir_sizes

processCmds(buildCmdTree day7testinput)
let dir_sizes_test = getAllDirectorySizes(root)

printfn "day7testpart1: %A" (dir_sizes_test |> List.filter(fun x -> x <= 100000) |> List.sum)

root = {name = "root"; size = 0; files = List.empty; children = List.empty; parent = None }
curDir = root

let day7input = System.IO.File.ReadAllText "day7.txt"
processCmds (buildCmdTree day7input)
let dir_sizes = getAllDirectorySizes(root)

printfn "day7part1: %A" (dir_sizes |> List.filter(fun x -> x <= 100000) |> List.sum)


let dirSizes(input: string) =
    input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    |> Array.fold (fun (path, directories) line -> 
        match line.Split(' ', StringSplitOptions.RemoveEmptyEntries) with
        | [| "$"; "cd"; ".." |] -> list.tail path, getAllDirectorySizes
        | [| "$"; "cd"; name |] -> name :: path, directories
        | [| num, _ -> |]
    ) 


let parse (paths, dirs: Map<string list, int64>) line =
    match line.Split(' ', StringSplitOptions.RemoveEmptyEntries) with
    | [| "$"; "cd"; ".." |] -> list.tail paths, directories
    | [| "$"; "cd"; name |] -> name :: paths, directories
    | [| num, _ |] -> 
    
    | Regex @"\$ cd \.\." [] -> List.tail paths, dirs
    | Regex @"\$ cd (.*)" [ dirName ] -> [ dirName ] @ paths, dirs
    | Regex @"([0-9]+) .*" [ fileSize ] ->
        let rec traverse (tail: string list) directories =
            match tail with
            | [] -> directories
            | _ :: t ->
                let newDirs =
                    Map.change tail (changeFunc (int64 fileSize)) directories

                traverse t newDirs

        paths, traverse paths dirs
    | _ -> paths, dirs