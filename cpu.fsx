open System

let splitLines (input: string) =
  input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)

let noop (register : List<int>) =
  0 :: register

let add (num: int) (register: List<int>) =
  num :: 0 :: register

let signalChecker (input: string) =
  input
  |> splitLines
  |> Array.fold (fun (x: List<int>) line ->
    let (x': List<int>) = 
      match line.Split(" ", StringSplitOptions.RemoveEmptyEntries) with
      | [|"noop"|] ->  (x |> noop)
      | [|"addx"; n|] -> x |> ((int n) |> add)
      | _ -> failwith (sprintf "invalid instruction %A" line)
    x'
  ) ([1])

let calcSignalStrength (x: List<int>)  (sct: List<int>) =
  sct
  |> List.map (fun period -> 
    x |> List.rev
    |> List.take period
    |> List.sum
  )
  |> List.map2 (fun x y -> x * y) sct
  |> List.sum

let drawPixel register position =
  let p' = (position % 40) |> abs
  if p' >= register - 1 && p' <= register + 1 then
    "#"
  else
    "."

let draw(register: List<int>) =
  let mutable r' = 1
  let crt = 
    [1..((register |> List.length) - 1)]
    |> List.map (fun i ->
      printfn "i: %A register[i]: %A r': %A" i register[i] r'
      let pixel = drawPixel r' i
      if register[i] <> 0 then
        r' <- register |> List.take (i + 1) |> List.sum
      pixel
      )
      |> List.chunkBySize(40)
      |> List.map (fun x -> String.concat "" x)
  Environment.NewLine + String.concat Environment.NewLine crt

let sct = [20; 60; 100; 140; 180; 220]
let testResult = signalChecker (System.IO.File.ReadAllText "day10test.txt")
let part1Result = signalChecker (System.IO.File.ReadAllText "day10.txt")

printfn "test: %A" (calcSignalStrength testResult sct)
printfn "Part 1: %A" (calcSignalStrength part1Result sct) 
printfn "Test"
printfn "test: %A" (draw (testResult |> List.rev))

    