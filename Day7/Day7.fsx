// we only care about the parents of step and which steps we've already completed
open System.Text.RegularExpressions

type Node =
  { Id: char
    Parents: char list }

  member n.addParent nodeId =
    { n with Parents = nodeId::n.Parents }


type Graph =
  { Nodes: Map<char, Node> }

  member g.addEdge fromNodeId toNodeId =

    printfn "%A -> %A" fromNodeId toNodeId

    let gWithFrom =
      match Map.containsKey fromNodeId g.Nodes with
      | true -> g
      | false ->
        { Nodes = Map.add fromNodeId { Id = fromNodeId; Parents = [] } g.Nodes }

    let gWithTo =
      match Map.containsKey toNodeId gWithFrom.Nodes with
      | true ->
        let toNode = Map.find toNodeId gWithFrom.Nodes
        { Nodes = Map.add toNodeId (toNode.addParent(fromNodeId)) gWithFrom.Nodes }
      | false ->
        { Nodes = Map.add toNodeId { Id = toNodeId; Parents = [ fromNodeId ] } gWithFrom.Nodes }

    gWithTo


type Rule =
  { Id: char
    Parent: char }

type Traversal =
  { ExploredNodes: char list
    RemainingSteps: char list }

  member t.explore (node: Node) =
    { t with
        ExploredNodes = node.Id::t.ExploredNodes;
        RemainingSteps =
          t.RemainingSteps
          |> List.filter (fun c -> not (c = node.Id)) }


// let input = @"C:\Users\john\code\Aoc2018\Day7\sampleInput.txt"
let input = @"C:\Users\john\code\Aoc2018\Day7\input.txt"

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
  |> List.sort

let ruleFolder (graph:Graph) (rule:Rule) =
  graph.addEdge rule.Parent rule.Id

let graph =
  rules
  |> Seq.fold ruleFolder  { Nodes = Map.empty }


let initialState =
  { ExploredNodes = []
    RemainingSteps = distinctIds }

let rec traverseGraph state graph =
  printfn "explored: %A" state.ExploredNodes
  printfn "remaining: %A" state.RemainingSteps
  //if the frontier only contains the destNode we're done
  match state.RemainingSteps with
  | [] -> state.ExploredNodes |> List.rev
  | remaining ->
    let remainingNodes =
      remaining
      |> List.map (fun stepId -> Map.find stepId graph.Nodes)

    //all parents need to be explored for us to explore a node
    //don't explore nodes we've already explored
    let nextNode =
      remainingNodes
      |> List.filter (fun node ->
        printfn "node %A has parents %A" node.Id node.Parents
        node.Parents
        |> List.forall (fun p -> List.contains p state.ExploredNodes))
      |> List.head
    printfn "doing step %A" nextNode.Id
    let newState = state.explore nextNode
    traverseGraph newState graph

let result = traverseGraph initialState graph

result
|> List.map string
|> String.concat ""
