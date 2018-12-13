open System

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


let rules = formatRules inputRules
let buffer state  bufferSize =
  let buf =
    seq { for i in {0..bufferSize - 1} -> "." }
    |> String.concat ""

  buf + state + buf


let bufferSize = 1000


let pots = getPots (buffer initialState bufferSize)

let finalResult =
  { 1..300 }
  |> Seq.fold (fun (s, last) i ->
    let result = applyRules rules s
    let sum =
      Seq.mapi (fun i c ->
        match c with
        | '#' -> i - bufferSize
        | '.' -> 0
        | _ -> failwith "weird char") result
      |> Seq.sum

    let diff = sum - last
    printfn "%A | gen %A| sum indices = %A diff = %A" result i sum diff
    let newPots = getPots result
    (newPots, sum)) (pots, 0)

//@ gen 250 sum = 19863 and increase by 75 forever
let finalResult = (50000000000L - 250L) * 75L + 19863L
//3750,000,001,113




