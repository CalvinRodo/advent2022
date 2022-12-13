open System

#load "utils.fsx"
open utils

let day11test = """
"""
let getMonkey (line: string) =
  line.ToCharArray()
  |> Array.rev
  |> Array.skipWhile (fun x-> x = ':')
  |> Array.head |> string |> int

let getOp op (val1: int64) (val2: int64) =
  match op with
  | "+" -> val1 + val2
  | "*" -> val1 * val2
  | _ -> failwith (sprintf "invalid op %A" op)

let parseOp line =
  match line |> splitWords with
    | [| _; op; "old"|] -> fun old -> getOp(op) old old
    | [| _; op; num|] -> fun old -> getOp(op) old (int64 num)
    | _ -> failwith (sprintf "invalid op line %A" line)

let parseOperation line =
  match line |> splitString " = " with
  | [|_; op|] -> op |> parseOp
  | _ -> failwith (sprintf "invalid op line %A" line)

type Monkey =
  class

    val num : int
    val mutable stack: List<int64>
    val operation : int64 -> int64
    val modulus : int64
    val test: (int * int)
    val mutable inspection: int64

    new (lines : string[]) = {

      num = 
        lines.[0] 
          |> splitWords
          |> Array.last
          |> getMonkey;

      stack= 
        lines.[1] 
        |> splitString ": "
        |> Array.last
        |> splitString ", "
        |> Array.map (fun x -> int64 x)
        |> Array.toList;

      operation = 
        lines.[2] 
        |> parseOperation;

      modulus =
        lines[3] 
        |> splitWords 
        |> Array.last 
        |> int64; 

      test = ( 
        lines[4] |> getMonkey, 
        lines[5] |> getMonkey
      )

      inspection = 0
    }
  end

let buildMonkey (lines: string[]) =
  new Monkey (lines)

//Parsing 
let parseMonkeyMap input =
  input
  |> splitParagraphs
  |> Array.map (fun line -> line |> splitLines |> buildMonkey)
  |> Array.map( fun monkey -> (monkey.num, monkey))
  |> Map.ofArray

let lcm (monkeys: Map<int, Monkey>) =
  monkeys 
  |> Map.values 
  |> Seq.map (fun x -> x.modulus)
  |> Seq.reduce (*)


let round (monkeys: Map<int, Monkey>) divisor (monkey: Monkey) =
  monkey.inspection <- monkey.inspection + (int64 monkey.stack.Length)
  while monkey.stack.Length <> 0 do
    let worry = ((monkey.operation monkey.stack.Head) / divisor) % lcm(monkeys)
    let (t, f) = monkey.test
    let index = if (worry % monkey.modulus) = 0L then t else f

    monkey.stack <- monkey.stack.Tail
    monkeys[index].stack <- worry :: monkeys[index].stack 

let sim num divisor mm =
  [1..num] 
  |> List.iter (fun i -> 
    mm 
    |> Map.iter (fun _ m -> m |> round mm divisor)
  )

  mm 
  |> Map.values 
  |> Seq.map (fun x -> x.inspection) 
  |> Seq.sortDescending 
  |> Seq.take 2 


let mmTest = parseMonkeyMap (IO.File.ReadAllText "day11test.txt")
let p1test = mmTest |> sim 20 3L
printfn "Part 1 Test: %A" (p1test)
printfn "Part 1 Test: %A" (p1test |> Seq.reduce (*))

let mm = parseMonkeyMap (IO.File.ReadAllText "day11.txt")
let p1 = mm |> sim 20 3L 
printfn "Part 1: %A" (p1)
printfn "Part 1: %A" (p1 |> Seq.reduce (*))

let mmTest2 = parseMonkeyMap (IO.File.ReadAllText "day11test.txt")
let p2test = mmTest2 |> sim 10000 1

printfn "Part 2 Test: %A" (p2test)
printfn "Part 2 Test: %A" (p2test |> Seq.reduce (*))

let mm2 = parseMonkeyMap (IO.File.ReadAllText "day11.txt")
let p2 = mm2 |> sim 10000 1

printfn "Part 2: %A" (p2)
printfn "Part 2: %A" (p2 |> Seq.reduce (*))
