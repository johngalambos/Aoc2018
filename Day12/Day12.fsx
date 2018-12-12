open System

type PotState = Empty | Full

type Pot =
  { Id: int
    State: PotState }

type PotList =
  { Posts: pot list }

let sampleState = "#..#.#..##......###...###"
let sampleRules =
  [ "...## => #"
    "..#.. => #"
    ".#... => #"
    ".#.#. => #"
    ".#.## => #"
    ".##.. => #"
    ".#### => #"
    "#.#.# => #"
    "#.### => #"
    "##.#. => #"
    "##.## => #"
    "###.. => #"
    "###.# => #"
    "####. => #"]
//add 2 pots to the start

let initialState = "#......##...#.#.###.#.##..##.#.....##....#.#.##.##.#..#.##........####.###.###.##..#....#...###.##"

let inputRules = [
  ".#.## => ."
  ".#### => ."
  "#..#. => ."
  "##.## => #"
  "..##. => #"
  "##... => #"
  "..#.. => #"
  "#.##. => ."
  "##.#. => ."
  ".###. => #"
  ".#.#. => #"
  "#..## => #"
  ".##.# => #"
  "#.### => #"
  ".##.. => #"
  "###.# => ."
  "#.#.# => #"
  "#.... => ."
  "#...# => ."
  ".#... => #"
  "##..# => ."
  "....# => ."
  "..... => ."
  ".#..# => #"
  "##### => ."
  "#.#.. => ."
  "..#.# => #"
  "...## => ."
  "...#. => #"
  "..### => ."
  "####. => #"
  "###.. => #"
]



let getPots (input:string) =
  input
  |> Seq.mapi (fun i c ->
    match i with
    | 0 -> ".." + input.[i..2]
    | 1 -> "." + input.[..3]
    | i when i = (input.Length - 1) -> input.[input.Length - 3 ..] + ".." //last element
    | i when i = (input.Length - 2) -> input.[input.Length - 4 ..] + "." //second last element
    | _ -> input.[i-2..i+2 ])

let formatRules (input: string list) =
  input
  |> List.map (fun s -> s.Split([|" => "|], StringSplitOptions.None))
  |> List.map (fun a -> (a.[0], a.[1]))

let applyRules rules pots =
  pots
  |> Seq.mapi (fun i pot ->
    let result =
      rules
      |> Seq.tryFind (fun (pattern, grows) -> pattern = pot)
    match result with
    | Some (pattern, grows) ->
      // printfn "matched %A with pot %A returning %A" pattern i grows
      grows
    | None ->
      // printfn "no match for pot %A with pattern %A" i pot
      ".") //assume nothing happens?
  |> String.concat ""


let rules = formatRules sampleRules
let pots = getPots sampleState

let rec appendEmptyPots (str:string) =
  if str.[str.Length - 2..] <> ".." then
    appendEmptyPots (str + ".")
  else
    str

let rec prependEmptyPots (str:string) =
  if str.[..1] <> ".." then
    prependEmptyPots ("." + str)
  else
    str

let appendPots str =
  let str = appendEmptyPots str
  prependEmptyPots str


let finalResult =
  { 1..20 }
  |> Seq.fold (fun s i ->
    let result = applyRules rules s
    //append empty pots to string if necessary
    let result = appendPots result
    printfn "%A | gen %A" result i
    let newPots = getPots result
    newPots) pots




