// parse the input
// they're a bunch of strings ordered by datetime
// loop through them populating a 60 width array of ints for each guard for each night
// each guard will get
// we can some the ints to get the total for each guard as well as find the max minute
open System.Text.RegularExpressions

type Interval =
  { guard: int
    date: System.DateTime
    startMin: int
    endMin: int }

let inputsTest = System.IO.File.ReadLines(@"C:\Users\john\code\Aoc2018\Day4\inputtest.txt")
let inputs = System.IO.File.ReadLines(@"C:\Users\john\code\Aoc2018\Day4\input.txt")

let parseDate dateString =
  System.DateTime.Parse dateString

let getDateAndString (input:string) =
  let dateString = input.[1..16]
  let remainingString = input.[19..]
  (parseDate dateString, remainingString)

let getGuardNum s =
  Regex.Match(s, "\d+").Value |> int

let sleepIntervals (results: seq<System.DateTime*string>) =
  let mutable sleepMin = 0
  let mutable guardNum = 0

  seq {
    for (d, r)  in results do
      match r with
      | "falls asleep" ->
        sleepMin <- d.Minute
      | "wakes up" ->
        let wakeMin = d.Minute
        yield
          { guard = guardNum
            date = d.Date
            startMin = sleepMin
            endMin = d.Minute - 1 }
      | s ->
        guardNum <- getGuardNum s }

let toSleepArray intervals =
  let sleepArray = Array.zeroCreate 60
  for i in intervals do
    for m in i.startMin .. i.endMin do
      sleepArray.[m] <- sleepArray.[m] + 1
  sleepArray

let orderedResults =
  inputs
  |> Seq.map getDateAndString
  |> Seq.sortBy (fun (d, s) -> d)

let intervalsByGuard =
  orderedResults
  |> sleepIntervals
  |> Seq.groupBy (fun i -> i.guard)
  |> Seq.map (fun (g, intervals) -> (g, toSleepArray intervals))

let mostSleep =
  intervalsByGuard
  |> Seq.maxBy (fun (g, pattern) -> Array.sum pattern)

let mostSleepMin pattern =
  let max = Array.max pattern
  Array.findIndex (fun i -> i = max) pattern


let mostSleepGuard = fst mostSleep
let mostSleptMin = mostSleepMin (snd mostSleep)

printfn "Most sleep guard: %A" mostSleepGuard
printfn "Most sleep min: %A" mostSleptMin
printfn "Answer: %A" (mostSleepGuard * mostSleptMin)

// guard 10 has the most sleep
// most common min is the index of the maximum int

// part 2 - guard who sleeps the most in any particular min
let maxMinuteData =
  intervalsByGuard
  |> Seq.maxBy (fun (g, pattern) -> Array.max pattern)


let indexOfMaxMinute pattern =
  let max = Array.max pattern
  Array.findIndex (fun i -> i = max) pattern

let maxMinuteGuard = fst maxMinuteData
let maxMinute = indexOfMaxMinute (snd maxMinuteData)

printfn "Max minute guard: %A" maxMinuteGuard
printfn "Max minute: %A" maxMinute
printfn "Answer: %A" (maxMinuteGuard * maxMinute)


