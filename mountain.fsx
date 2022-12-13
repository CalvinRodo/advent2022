// Advent of Code 2022 - Day 12
open System.Collections.Generic
open System

let input =
    System.IO.File.ReadAllLines "day12.txt"
    |> Array.map (fun line -> line.ToCharArray())

type Node = { visited: bool; distance: int }

let DefaultNode =
    { visited = false
      distance = Int32.MaxValue }

let find2D target arr =
    let row = arr |> Array.findIndex (Array.contains target)
    row, Array.findIndex (fun c -> c = target) arr[row]

let start = find2D 'S' input
let target = find2D 'E' input

let elevation (x, y) =
    match input[x][y] with
    | 'S' -> 0
    | 'E' -> (int 'z') - (int 'a')
    | c -> (int c) - (int 'a')

let adjacent (x, y) =
    [ (x - 1, y)
      (x + 1, y)
      (x, y - 1)
      (x, y + 1) ]

let inbounds graph (x, y) =
    x >= 0
    && y >= 0
    && x < Array2D.length1 graph
    && y < Array2D.length2 graph

let dijkstra cost width height source target =
    let graph = Array2D.create width height DefaultNode

    let (sx, sy) = source
    graph.[sx, sy] <- { graph.[sx, sy] with distance = 0 }

    let queue = PriorityQueue<int * int, int>()
    queue.Enqueue(source, 0)

    let dist (x, y) = graph[x, y].distance
    let isVisited (x, y) = graph[x, y].visited

    let rec visit () =
        if queue.Count = 0 then
            dist target
        else
            match queue.Dequeue() with
            | node when isVisited node -> visit ()
            | node when node = target -> dist node
            | (x, y) ->
                graph[x, y] <- { graph[x, y] with visited = true }

                adjacent (x, y)
                |> Seq.filter (fun n -> inbounds graph n && not (isVisited n))
                |> Seq.filter (fun n -> elevation n <= elevation (x, y) + 1)
                |> Seq.map (fun n -> (n, dist (x, y) + cost n))
                |> Seq.filter (fun (n, d) -> d < dist n)
                |> Seq.iter (fun ((xn, yn), d) ->
                    graph[xn, yn] <- { graph[xn, yn] with distance = d }
                    queue.Enqueue((xn, yn), d))

                visit ()

    visit ()

let width = Array.length input
let height = Array.length input[0]

let part1 = dijkstra (fun _ -> 1) width height start target

printfn "Part 1: %A" part1

let part2 =
    [ for row in 0 .. width - 1 do
          for col in 0 .. height - 1 do
              if elevation (row, col) = 0 then
                  dijkstra (fun _ -> 1) width height (row, col) target ]
    |> List.min

printfn "Part 2: %A" part2
