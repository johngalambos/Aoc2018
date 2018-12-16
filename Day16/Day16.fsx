open System.Text.RegularExpressions

type Sample =
  { Before: int list
    Instruction: int list
    After: int list }

let addr reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- reg.[i1] + reg.[i2]
  Array.toList regArr

let addi reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- reg.[i1] + i2
  Array.toList regArr

let mulr reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- reg.[i1] * reg.[i2]
  Array.toList regArr

let muli reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- reg.[i1] * i2
  Array.toList regArr

let banr reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- reg.[i1] &&& reg.[i2]
  Array.toList regArr

let bani reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- reg.[i1] &&& i2
  Array.toList regArr

let borr reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- reg.[i1] ||| reg.[i2]
  Array.toList regArr

let bori reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- reg.[i1] ||| i2
  Array.toList regArr

let setr reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- reg.[i1]
  Array.toList regArr

let seti reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- i1
  Array.toList regArr

let gtir reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- if i1 > reg.[i2] then 1 else 0
  Array.toList regArr

let gtri reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- if reg.[i1] > i2 then 1 else 0
  Array.toList regArr

let gtrr reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- if reg.[i1] > reg.[i2] then 1 else 0
  Array.toList regArr

let eqir reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- if i1 = reg.[i2] then 1 else 0
  Array.toList regArr

let eqri reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- if reg.[i1] = i2 then 1 else 0
  Array.toList regArr

let eqrr reg [op; i1; i2; out] =
  let regArr = List.toArray reg
  regArr.[out] <- if reg.[i1] = reg.[i2] then 1 else 0
  Array.toList regArr

let ops = [ addr; addi; mulr; muli; banr; bani; borr; bori; setr; seti; gtir; gtri; gtrr; eqir; eqri; eqrr]

let matching sample ops =
  Seq.fold (fun s op ->
    let actual = op sample.Before sample.Instruction
    // printfn "before %A actual %A seek %A" before actual after
    if sample.After = actual then s + 1 else s)
    0 ops

let before = [3; 2; 1; 1]
let after = [3; 2; 2; 1]
let instruction = [9; 2; 1; 2]


// let m = beforeRegex.Match("Before: [3, 0, 0, 3]")


let matchToIntList (mtch: Match) =
  Seq.map
    (fun (g:Group) -> g.Value |> int)
    (mtch.Groups |> Seq.cast<Group> |> Seq.skip 1)
  |> Seq.toList

let beforeRegex = new Regex(@"^Before: \[(\d), (\d), (\d), (\d)\]$")
let instRegex = new Regex("^(\d+) (\d) (\d) (\d)$")
let afterRegex = new Regex("^After:  \[(\d), (\d), (\d), (\d)\]$")

let rec parseInput (input: string list) parsed =
  match input with
  | [] -> parsed
  | bs::is::afs::_::t ->
    let parsed =
      { Before = beforeRegex.Match(bs) |> matchToIntList
        Instruction = instRegex.Match(is) |> matchToIntList
        After = afterRegex.Match(afs) |> matchToIntList } :: parsed
    parseInput t parsed
  | _ -> failwith "should not have made it here"


let input = System.IO.File.ReadLines("C:\Users\john\code\Aoc2018\Day16\input1.txt") |> Seq.toList
let samples = parseInput input []

let solve opCodeMatchCount samples =
  samples
  |> Seq.fold (fun state s ->
    match (matching s ops) with
    | total when total >= opCodeMatchCount -> state + 1
    | _ -> state) 0

solve 3 samples

// let result = matching before instruction after

