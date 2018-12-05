// seq of strings
// parse string to int
// sum the list of ints

let input1 = 
  ["0"; "-2"; "+3"; "+1"]


let parseInt (s:string) = 
  s |> int


let sum input =
  input
  |> List.map parseInt
  |> List.sum


let lines = System.IO.File.ReadAllLines(@"C:\Users\john\code\Aoc2018\Day1\input1.txt")

let linesParsed =
  lines
  |> List.ofArray
  |> List.map int

let input1Parsed = 
  input1
  |> List.map parseInt

// has the new frequency already been seen?
// keep unfolding values from the list until we find a duplicate value

//Seq.un
// start over if we're at the end of the original list and haven't found a duplicate

let rec fold freqs lastFreq deltas initialList =
  match deltas with 
  | [] -> 
      printfn "empty list starting over"
      fold freqs lastFreq initialList initialList
  | h::t -> 
      let newFreq = lastFreq + h
      match Map.containsKey newFreq freqs with
      | true -> 
          printfn "found a duplicate frequency"
          newFreq
      | false ->
          printfn "trying again"
          fold (Map.add newFreq newFreq freqs) newFreq t initialList











