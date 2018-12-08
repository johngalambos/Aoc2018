open System.Text.RegularExpressions

type Node =
  { Id: char
    Children: char list }

  member n.addChild nodeId =
    { n with Children = nodeId::n.Children }

type Graph =
  { Nodes: Map<char, Node> }

  member g.addEdge fromNodeId toNodeId =

    let withFrom =
      match Map.containsKey fromNodeId g.Nodes with
      | true ->
        let fromNode = Map.find fromNodeId g.Nodes
        { Nodes = Map.add fromNodeId (fromNode.addChild(toNodeId)) g.Nodes }
      | false ->
        { Nodes = Map.add fromNodeId { Id = fromNodeId; Children = [ toNodeId ] } g.Nodes }

    let withTo =
      match Map.containsKey toNodeId withFrom.Nodes with
      | true -> withFrom
      | false -> { Nodes = Map.add toNodeId { Id = toNodeId; Children = [] } withFrom.Nodes }

    withTo


type Rule =
  { Id: char
    Parent: char }

let input = @"C:\Users\john\code\Aoc2018\Day7\sampleInput.txt"
// let input = @"C:\Users\john\code\Aoc2018\Day7\input.txt"

let parseRuleFromText text =
  let result = Regex.Match(text, "^Step (.) must be finished before step (.) can begin.$")
  { Id = result.Groups.[2].Value.ToCharArray().[0]
    Parent = result.Groups.[1].Value.ToCharArray().[0] }

let rules =
  System.IO.File.ReadLines(input)
  |> Seq.map parseRuleFromText

let distinctIds =
  let parentList =
    rules
    |> Seq.map (fun r -> r.Parent)
    |> Seq.distinct

  let childList =
    rules
    |> Seq.map (fun r -> r.Id)
    |> Seq.distinct

  Seq.append parentList childList
  |> Seq.distinct
  |> List.ofSeq

// the root node is the one with no parent
// if we go through the ruls there will be no rule with this as Id
let rootNode =
  let childList =
    rules
    |> Seq.map (fun r -> r.Id)
    |> Seq.distinct

  distinctIds
  |> List.find (fun id -> (Seq.contains id childList |> not))

let destNode =
  let parentList =
    rules
    |> Seq.map (fun r -> r.Parent)
    |> Seq.distinct

  distinctIds
  |> List.find (fun id -> (Seq.contains id parentList |> not))


let ruleFolder (graph:Graph) (rule:Rule) =
  graph.addEdge rule.Parent rule.Id


let graph =
  rules
  |> Seq.fold ruleFolder  { Nodes = Map.empty }


// starting from the root node and going to the dest node at every iteration we
// choose the next lowest item, drop it from the frontier and add all its
// children
// we only stop when the frontier contains the destination

// let rec traverseGraph currentNodeId destNodeId explored frontier graph =
//   let currentNode = Map.find currentNodeId graph.Nodes
//   let newFrontier
