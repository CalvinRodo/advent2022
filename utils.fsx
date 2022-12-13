module utils 

open System
let splitLines (input: string) =
  input.Trim().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)

let splitParagraphs (input: string) =
  input.Trim().Split(Environment.NewLine + Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)

let splitWords (input: string) =
  input.Trim().Split(" ", StringSplitOptions.RemoveEmptyEntries)

let splitString (token: string) (input: string) =
  input.Trim().Split(token, StringSplitOptions.RemoveEmptyEntries)
