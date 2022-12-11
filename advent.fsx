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
open System.Text.RegularExpressions

let (|Regex|_|) pattern s =
    let m = Regex.Match(s, pattern)

    match m.Success with
    | false -> None
    | true -> Some(List.tail [ for g in m.Groups -> g.Value ])

let changeFunc num value =
    match value with
    | None -> Some(int64 num)
    | Some x -> Some(num + int64 x)

let parse (paths, dirs: Map<string list, int64>) line =
    match line with
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

let getDirectorySizes fn =
    System.IO.File.ReadLines("day7.txt")
    |> Seq.fold parse ([], Map.empty<string list, int64>)
    |> snd
    |> Map.values
    
let part1 fn () =
    getDirectorySizes fn
    |> Seq.filter ((>) 100000)
    |> Seq.sum

let part2 fn () =
    let dirSizes = getDirectorySizes fn
    let total = 70_000_000L
    let needed = 30_000_000L
    let toBeFreed = needed - (total - (Seq.max dirSizes))
    dirSizes |> Seq.filter ((<) toBeFreed) |> Seq.min

printfn "%A" ((part1 ())())
printfn "%A" ((part2 ())())


printfn "Day 8"

let day8test = """30373
25512
65332
33549
35390"""

let getAllVisibleTrees (line: int []) =
    line
    |> Array.mapFold (fun max height -> max < height, Math.Max(max, height)) -1
    |> fst

let calcScenic line =
    let indexed = Array.indexed line

    indexed
    |> Array.map (fun (i, height) ->
        indexed
        |> Seq.take i
        |> Seq.rev
        |> Seq.takeWhile (fun (_, h) -> h < height)
        |> Seq.length
        |> fun x -> if x = i then x else x + 1)

let map2d2 (arr1: 'T[][]) (arr2: 'T[][]) f  =
    let at: 'T[][] = Array.create arr1.Length (Array.zeroCreate arr1.Length)
    for i in 0 .. arr1.Length - 1 do
        at[i] <- Array.map2 f arr1.[i] arr2.[i]
    at

let orTwo2dArrays arr1 arr2 =
    map2d2 arr1 arr2 (||)

let multTwo2dArrays arr1  arr2=
    map2d2 arr1 arr2 (*)

let toCharArray(input : string) =
    input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map(fun x -> x.ToCharArray())
    |> Array.map(fun x -> x |> Array.map(fun y -> y |> string |> int))

let getAllScenic(arr: int[][]) =
    let l = arr |> Array.map(fun x -> x |> calcScenic)
    let r = arr |> Array.map(fun x -> x |> Array.rev |> calcScenic |> Array.rev) 
    let t = arr |> Array.transpose |> Array.map(fun x -> x |> calcScenic) |> Array.transpose
    let b = arr |> Array.transpose |> Array.map(fun x -> x |> Array.rev |> calcScenic |> Array.rev) |> Array.transpose

    let lr = multTwo2dArrays l r
    let tb = multTwo2dArrays t b
    let all = multTwo2dArrays lr tb 

    all |> Array.map(fun x -> x |> Array.max) |> Array.max

let getAllVisible(arr: int[][]) =
    let l = arr |> Array.map(fun x -> x |> getAllVisibleTrees)
    let r = arr |> Array.map(fun x -> x |> Array.rev |> getAllVisibleTrees |> Array.rev) 
    let t = arr |> Array.transpose |> Array.map(fun x -> x |> getAllVisibleTrees) |> Array.transpose
    let b = arr |> Array.transpose |> Array.map(fun x -> x |> Array.rev |> getAllVisibleTrees |> Array.rev) |> Array.transpose

    let lr = orTwo2dArrays l r
    let tb = orTwo2dArrays t b
    let all = orTwo2dArrays lr tb

    all |> Array.map(fun x -> x |> Array.filter((=) true) |> Array.length) |> Array.sum
    
let countVisible(input: string) =
    toCharArray(input)
    |> getAllVisible

let getScenic(input: string ) =
    toCharArray(input)
    |> getAllScenic

printfn "Part1: %A" (countVisible (System.IO.File.ReadAllText "day8.txt"))
printfn "Part2: %A" (getScenic (System.IO.File.ReadAllText "day8.txt"))


printfn "Day 9"



let calcDelta (move : string) =
    match move.Split(" ", StringSplitOptions.RemoveEmptyEntries) with
    | [|"R"; d |] -> (int d, 0)
    | [|"L"; d |] -> (-(int d), 0)
    | [|"U"; d |] -> (0, int d)
    | [|"D"; d |] -> (0, -(int d))
    | _ -> failwith "Invalid input"

let addTuple (a: int * int) (b: int * int) =
    match (a, b) with
    | ((x1, y1), (x2, y2)) -> (x1 + x2, y1 + y2)

let getNextMove (h: int * int) (t: int * int) (d : (int * int)) =
    match (addTuple h d, t) with
    | ((dx, dy), (tx, ty)) when dx = tx && dy = ty -> (0,0)

    | ((dx, dy), (tx, ty)) when dx > tx && dy = ty -> addTuple t (1,0) // L
    | ((dx, dy), (tx, ty)) when dx < tx && dy = ty -> addTuple t (-1,0) // R
    | ((dx, dy), (tx, ty)) when dx = tx && dy < ty -> addTuple t (0,-1)  // D
    | ((dx, dy), (tx, ty)) when dx = tx && dy > ty -> addTuple t (0,1) // U

    | ((dx, dy), (tx, ty)) when dx > tx && dy > ty -> addTuple t (1,1) //RU
    | ((dx, dy), (tx, ty)) when dx > tx && dy < ty -> addTuple t (1,-1) //RD

    | ((dx, dy), (tx, ty)) when dx < tx && dy < ty -> addTuple t (-1,-1) //LD
    | ((dx, dy), (tx, ty)) when dx < tx && dy > ty -> addTuple t (-1,1) //LU

    | _ -> failwith "Invalid input"

let calcPath (h: int * int) (t: int * int) (d : (int * int)) =
    let h' = addTuple h d
    let rec traverse (h: int * int) (t: int * int) (path) =
        let move = getNextMove h t d
        if move = h || move = (0,0) then
            path
        else
            traverse h' move (path @ [move])
    traverse h' t []


let third (_, _, c) = c
let getDeltas (input: string) =
    input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x |> calcDelta)

let acc (h: int * int, t: int * int, path: list<(int * int)>) d = 
    //printfn "h: %A, t: %A, path: %A" h t path
    let h' = addTuple h d
    let path' = path @ calcPath h t d
    let t' = path' |> List.rev |> List.head
    //printfn "h': %A, t': %A, path': %A" h' t' path'
     
    (h', t', path')

let part1Day9 (input: string) = 
    input 
    |> getDeltas
    // convert the deltas to a list of (h, t, path) tuples
    |> Array.fold (acc) ((0,0), (0,0), [(0,0)])
    |> third

let day9test = """
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""

printfn "Part1TestData: %A" (part1Day9 day9test |> List.distinct |> List.length)
printfn "Part 1: %A" (part1Day9 (System.IO.File.ReadAllText "day9.txt") |> List.distinct |> List.length)

let getInput (input: string)=
    input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> (x.Split(" ", StringSplitOptions.RemoveEmptyEntries)))
    |> Array.map (fun x -> (x.[0], int x.[1]))

let follow (hx, hy) (tx, ty) =
    match hx - tx, hy - ty with
    | x, y when abs x <= 1 && abs y <= 1 -> tx, ty
    | x, y -> tx + sign x, ty + sign y

let moveRope (dx, dy) rope =
    match rope with
    | [] -> []
    | (hx, hy) :: t -> List.scan follow (hx + dx, hy + dy) t

let delta direction =
    match direction with
    | "U" -> 0, 1
    | "D" -> 0, -1
    | "R" -> 1, 0
    | "L" -> -1, 0
    | _ -> failwith "Invalid direction"

let visitedByTail moves rope =
    moves
    |> Array.fold
        (fun (visited, rope) (dir, amount) ->
            Seq.init amount id
            |> Seq.fold
                (fun (set, rope) _ ->
                    let newRope = moveRope (delta dir) rope
                    Set.add (List.last newRope) set, newRope)
                (visited, rope))
        (Set.singleton (0, 0), rope)
    |> fst
    |> Set.count

let makeRope ropeSize = List.init ropeSize (fun _ -> 0, 0)

let cheat1 = makeRope 2 |> visitedByTail (getInput (System.IO.File.ReadAllText "day9.txt"))
let cheat2 = makeRope 10 |> visitedByTail (getInput (System.IO.File.ReadAllText "day9.txt"))
printfn "cheat 1: %A" cheat1
printfn "cheat 2: %A" cheat2