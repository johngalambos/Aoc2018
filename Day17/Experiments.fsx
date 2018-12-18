type Node =
  { mutable Parent: Node option
    mutable State: int
    mutable Children: seq<Node> }


let frontier = []
let explored = []


let n1 = { Parent = None; State = 1; Children=[] }
let frontier2 = n1::frontier
let explored2 = n1::explored

printfn "n1 %A" n1
printfn "frontier2 %A" frontier2
printfn "explored2 %A" explored2

n1.State <- 2
printfn "n1b %A" n1
printfn "frontier after mut %A" frontier2
printfn "explored after mut %A" explored2
n1.State <- 5
