open System

let splitLines (input: string) =
  input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)

let noop (register : List<int>) =
  register.Head :: register

let add (num: int) (register: List<int>) =
  (register.Head + num) :: register.Head :: register

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

let calcSignalStrength (register: List<int>)  (sct: List<int>) =
  sct
  |> List.map (fun period -> register[period] * period)
  |> List.sum

let draw(register: List<int>) =
  let crt = 
    register
    |> List.rev
    |> List.indexed
    |> List.map(fun (i, r) -> 
        let check = r % 40
        if check >= (i % 40) - 1 && check <= (i % 40) + 1 then
          "#"
        else
          "."
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
printfn "test: %A" (draw (testResult))
printfn "test: %A" (draw (part1Result))

    