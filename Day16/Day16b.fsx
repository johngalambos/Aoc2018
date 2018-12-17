open System.Text.RegularExpressions

type Sample =
  { Before: int list
    Instruction: int list
    After: int list }

type Instruction =
  { Name: string
    Fn: int list -> int list -> int list }

type MatchedSample =
  { Sample: Sample
    Matches: Instruction list }


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

let ops = [
  { Name = "addr"; Fn = addr }
  { Name = "addi"; Fn = addi }
  { Name = "mulr"; Fn = mulr }
  { Name = "muli"; Fn = muli }
  { Name = "banr"; Fn = banr }
  { Name = "bani"; Fn = bani }
  { Name = "borr"; Fn = borr }
  { Name = "bori"; Fn = bori }
  { Name = "setr"; Fn = setr }
  { Name = "seti"; Fn = seti }
  { Name = "gtir"; Fn = gtir }
  { Name = "gtri"; Fn = gtri }
  { Name = "gtrr"; Fn = gtrr }
  { Name = "eqir"; Fn = eqir }
  { Name = "eqri"; Fn = eqri }
  { Name = "eqrr"; Fn = eqrr } ]

let matchSample sample (ops: Instruction list) =
  Seq.fold (fun mSamp op ->
      let actual = op.Fn sample.Before sample.Instruction
      if sample.After = actual then
        { mSamp with Matches = op::mSamp.Matches }
      else
        mSamp)
    { Sample = sample; Matches = []} ops


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

let parseInputPart2 input =
  instRegex.Match(input) |> matchToIntList


let input = System.IO.File.ReadLines("C:\Users\john\code\Aoc2018\Day16\input1.txt") |> Seq.toList
let inputPart2 = System.IO.File.ReadLines("C:\Users\john\code\Aoc2018\Day16\input2.txt")
let samples = parseInput input []

// let getSampleWithFewestMatches samples nameOpcodeMap =
let pruneMatchedSamples matchedSamples (name, opcode) =
  //remove the samples we now know the answer for
  let matchedSamples =
    matchedSamples
    |> List.filter(fun ms -> ms.Sample.Instruction.[0] <> opcode)
  //remove this opcode from all samples where it matched by luck
  let matchedSamples =
    matchedSamples
    |> List.map (fun ms -> { ms with Matches = ms.Matches |> List.filter (fun inst -> inst.Name <> name) })

  matchedSamples

let testGuess opfn opcode (samples: Sample list) =
  printfn "testing guess for opcode %A against %A samples" opcode samples.Length
  samples
  |> List.filter (fun s -> s.Instruction.[0] = opcode)
  |> List.forall (fun s -> opfn s.Before s.Instruction = s.After)


let rec matchOpCodes matchedSamples samples ops nameOpcodeMap =
  match matchedSamples with
  | [] -> nameOpcodeMap
  |  _ ->
    printfn "Starting a new sample. Lookup so far %A" nameOpcodeMap
    let simplestNext =
      matchedSamples
      |> Seq.sortBy (fun ms -> ms.Matches.Length)
      |> Seq.head

    match simplestNext.Matches.Length with
    | 1 ->
      printfn "Sample with only 1 match Skipping the guessing phase"
      let matched = simplestNext.Matches.Head
      let nameOpcodeMap = Map.add simplestNext.Sample.Instruction.[0] matched.Fn  nameOpcodeMap
      let matchedSamples = pruneMatchedSamples matchedSamples (matched.Name, simplestNext.Sample.Instruction.[0])
      matchOpCodes matchedSamples  samples ops nameOpcodeMap
    | _ ->
      printfn "Doing some guessing"
      //make sure there's only one correct candidate in case they were being devious
      let tests = List.map (fun i -> (testGuess i.Fn simplestNext.Sample.Instruction.[0] samples, i)) simplestNext.Matches

      let singleInstruction =
        tests
        |> List.filter (fun (result, _) -> result = true)
        |> List.exactlyOne
        |> snd

      let nameOpcodeMap = Map.add simplestNext.Sample.Instruction.[0] singleInstruction.Fn nameOpcodeMap
      let matchedSamples = pruneMatchedSamples matchedSamples (singleInstruction.Name, simplestNext.Sample.Instruction.[0])
      matchOpCodes matchedSamples  samples ops nameOpcodeMap



let solve ops samples nameOpcodeMap =
  let matchedSamples =
    samples
    |> Seq.map (fun s -> matchSample s ops)
    |> Seq.toList

  matchOpCodes matchedSamples samples ops nameOpcodeMap

let opcodeFnLookup = solve ops samples Map.empty

printfn "%A" opcodeFnLookup

let part2Answer =
  inputPart2
  |> Seq.map parseInputPart2
  |> Seq.fold (fun (s:int list) (i:int list) ->
      let instr = Map.find i.[0] opcodeFnLookup
      let output = instr s i
      // printfn "output: %A" output
      output) [0;0;0;0]


