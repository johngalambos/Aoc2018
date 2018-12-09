// let input = System.IO.File.ReadAllText(@"C:\Users\john\code\Aoc2018\Day8\sampleInput.txt")
let input = System.IO.File.ReadAllText(@"C:\Users\john\code\Aoc2018\Day8\input.txt")
let entries = input.Split([|' '|]) |> List.ofArray |> List.map int

type Node =
    { Metadata: int list
      Children: Node list }

let parseMetadata (n: int) (src: int list) =
  (src.[n..], (List.take n src))

let rec parseNodes (n: int) (src: int list) : int list * Node list =
  List.fold
    (fun state _ ->
      let (src, nodes) = state
      match src with
      | [] ->
        (src, nodes)
      | 0::m::t ->
        let src, metadata = parseMetadata m t
        (src, { Metadata = metadata
                Children = [] } :: nodes)
      | n::m::t ->
        let src, newNodes = parseNodes n t
        let src, metadata = parseMetadata m src
        (src, { Metadata = metadata
                Children = newNodes } :: nodes)
      | _ -> failwith "catch this")
    (src, [])
    [1 .. n]


let parseTree src =
  match src with
  | n::m::t ->
    let src, nodes = parseNodes n t
    let _, metadata = parseMetadata m src
    { Metadata = metadata
      Children = nodes }
  | _ -> failwith "not sure what to do"

let tree = parseTree entries


let rec traverseTree tree =
  let sumLocal = List.sum tree.Metadata
  let sumChildren = List.sumBy (fun t -> traverseTree t) tree.Children
  sumLocal + sumChildren


let answer = traverseTree tree

let rec getNodeValue node =
  printfn "processing node with metadata %A and %A children" node.Metadata (List.length node.Children)
  // we have to reverse children due to the nature of how we built the tre
  match node.Children |> List.rev, node.Metadata with
  | [], metadata ->
    printfn "no children: summing metadata"
    List.sum metadata
  | children, metadata ->
    metadata
    |> List.sumBy (fun index ->
      printfn "summing values for node at index %A with %A children and metadata %A" index (List.length children) metadata
      match index with
      | 0 ->
        printfn "index of 0 returning 0"
        0
      | i when i <= (List.length children) ->
        let node = children.[i - 1]
        printfn "recursing into child node"
        let nodeValue = getNodeValue node
        printfn "node value was %A" nodeValue
        nodeValue
      | _ ->
        printfn "index higher than number of nodes in list, returning 0"
        0)

let answer2 = getNodeValue tree

