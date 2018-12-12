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

let rules = [
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
//we need to ensure that every rule only happens on the

let getFives (input:string) =
  input
  |> Seq.mapi (fun i c ->
    match i with
    | 0 -> ".." + input.[i..2]
    | 1 -> "." + input.[..3]
    | i when i = (input.Length - 1) -> input.[input.Length - 3 ..] + ".." //last element
    | i when i = (input.Length - 2) -> input.[input.Length - 4 ..] + "." //second last element
    | _ -> input.[i-2..i+2 ])

let pots = getFives sampleState

pots
|> Seq.iter (fun c -> printfn "%A" c)
