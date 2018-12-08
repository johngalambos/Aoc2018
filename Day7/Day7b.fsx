// we only care about the parents of step and which steps we've already completed
open System.Text.RegularExpressions

type StepState = Queued | InProgress | Done

type Step =
  { Id: char
    Parents: char list
    State: StepState }

  member n.addParent nodeId =
    { n with Parents = nodeId::n.Parents }


type Plan =
  { Steps: Map<char, Step> }

  member g.addStep parentStepId stepId =

    printfn "%A -> %A" parentStepId stepId

    let pWithFrom =
      match Map.containsKey parentStepId g.Steps with
      | true -> g
      | false ->
        { Steps = Map.add parentStepId { Id = parentStepId; Parents = []; State = Queued } g.Steps }

    let pWithTo =
      match Map.containsKey stepId pWithFrom.Steps with
      | true ->
        let toNode = Map.find stepId pWithFrom.Steps
        { Steps = Map.add stepId (toNode.addParent(parentStepId)) pWithFrom.Steps }
      | false ->
        { Steps = Map.add stepId { Id = stepId; Parents = [ parentStepId ]; State = Queued } pWithFrom.Steps }

    pWithTo

  member g.markInProgressStep stepId =
    let node = Map.find stepId g.Steps
    { Steps = Map.add stepId { node with State = InProgress } g.Steps }

  member g.markInProgress (stepIds: char list) =
    List.fold (fun (plan: Plan) stepId -> plan.markInProgressStep stepId) g stepIds

  member g.markCompleteStep stepId =
    let node = Map.find stepId g.Steps
    { Steps = Map.add stepId { node with State = Done } g.Steps }

  member g.markCompleteSteps stepIds =
    List.fold (fun (plan:Plan) stepId -> plan.markCompleteStep stepId) g stepIds


  member g.QueuedReadySteps =
    g.Steps
    |> Map.toList
    |> List.map (fun (key, step) -> step)
    |> List.filter (fun step -> step.State = Queued)
    |> List.filter (fun step ->
      step.Parents
      |> List.map (fun pid -> Map.find pid g.Steps)
      |> List.forall (fun p -> p.State = Done))



type Rule =
  { Id: char
    Parent: char }

type Traversal =
  { CompleteSteps: char list
    RemainingSteps: char list }

  member t.explore (node: Step) =
    { t with
        CompleteSteps = node.Id::t.CompleteSteps;
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

let ruleFolder (graph:Plan) (rule:Rule) =
  graph.addStep rule.Parent rule.Id

let graph =
  rules
  |> Seq.fold ruleFolder  { Steps = Map.empty }


type Assignment =
  { StepId: char
    DoneAt: int }

type SimState = {
  Graph: Plan
  Assignments: Assignment list
  Elapsed: int }

let workerPoolSize = 5

let getStepLength (step: Step) =
  let indexes = ['A' .. 'Z']
  (List.findIndex (fun id -> step.Id = id) indexes) + 61

let rec tick simState =
  match simState.Elapsed, simState.Assignments with
  | t, [] when t > 0 -> simState
  | _, _ ->
    //what assigments were just done
    let completedAssignments =
      simState.Assignments
      |> List.filter (fun a -> a.DoneAt = simState.Elapsed)

    let updatedGraph =
      simState.Graph.markCompleteSteps (completedAssignments
                                   |> List.map (fun a -> a.StepId))

    let ongoingAssignments =
      simState.Assignments |> List.except completedAssignments

    let availableWorkers = workerPoolSize - List.length ongoingAssignments

    let availableTasks = updatedGraph.QueuedReadySteps

    let capacity = List.min [ availableWorkers; List.length availableTasks]

    let tasksToStart = List.take capacity availableTasks

    let newPlan = updatedGraph.markInProgress (tasksToStart |> List.map (fun t -> t.Id))

    let newAssignments =
      tasksToStart |> List.map (fun t -> { StepId = t.Id; DoneAt = simState.Elapsed + (getStepLength t) })

    newAssignments
    |> List.iter (fun a -> printfn "assigning step %A which should be done at %A" a.StepId a.DoneAt)

    tick  { Graph = newPlan
            Assignments = newAssignments @ ongoingAssignments
            Elapsed = simState.Elapsed + 1 }

let finalState = tick { Graph = graph
                        Assignments = []
                        Elapsed = 0 }

printfn "total time is %A" (finalState.Elapsed - 1)
