open System.Text.RegularExpressions

type Op =
  { Alias: string
    Fn: int64 list -> int64 list -> int64 list }

type Instruction =
  { Name: string
    Fn: int64 list -> int64 list -> int64 list;
    Input: int64 list }

let addr reg ([op; i1; i2; out]: int64 list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] + reg.[i2 |> int]
  Array.toList regArr

let addi reg ([op; i1; i2; out]: int64 list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] + i2
  Array.toList regArr

let mulr reg ([op; i1; i2; out] : int64 list)=
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] * reg.[i2 |> int]
  Array.toList regArr

let muli reg ([op; i1; i2; out]: int64 list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] * i2
  Array.toList regArr

let banr reg ([op; i1; i2; out]: int64 list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] &&& reg.[i2 |> int]
  Array.toList regArr

let bani reg ([op; i1; i2; out]: int64 list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] &&& i2
  Array.toList regArr

let borr reg ([op; i1; i2; out]: int64 list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] ||| reg.[i2 |> int]
  Array.toList regArr

let bori reg ([op; i1; i2; out]: int64 list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] ||| i2
  Array.toList regArr

let setr reg ([op; i1; i2; out]: int64 list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int]
  Array.toList regArr

let seti reg ([op; i1; i2; out]: int64 list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- i1
  Array.toList regArr

let gtir reg ([op; i1; i2; out]: int64 list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- if i1 > reg.[i2 |> int] then 1L else 0L
  Array.toList regArr

let gtri reg ([op; i1; i2; out]: int64 list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- if reg.[i1 |> int] > i2 then 1L else 0L
  Array.toList regArr

let gtrr reg ([op; i1; i2; out] : int64 list)=
  let regArr = List.toArray reg
  regArr.[out |> int] <- if reg.[i1 |> int] > reg.[i2 |> int] then 1L else 0L
  Array.toList regArr

let eqir reg ([op; i1; i2; out] : int64 list)=
  let regArr = List.toArray reg
  regArr.[out |> int] <- if i1 = reg.[i2 |> int] then 1L else 0L
  Array.toList regArr

let eqri reg ([op; i1; i2; out] : int64 list)=
  let regArr = List.toArray reg
  regArr.[out |> int] <- if reg.[i1 |> int] = i2 then 1L else 0L
  Array.toList regArr

let eqrr reg ([op; i1; i2; out] : int64 list)=
  let regArr = List.toArray reg
  regArr.[out |> int] <- if reg.[i1 |> int] = reg.[i2 |> int] then 1L else 0L
  Array.toList regArr

let ops = [
  { Alias = "addr"; Fn = addr }
  { Alias = "addi"; Fn = addi }
  { Alias = "mulr"; Fn = mulr }
  { Alias = "muli"; Fn = muli }
  { Alias = "banr"; Fn = banr }
  { Alias = "bani"; Fn = bani }
  { Alias = "borr"; Fn = borr }
  { Alias = "bori"; Fn = bori }
  { Alias = "setr"; Fn = setr }
  { Alias = "seti"; Fn = seti }
  { Alias = "gtir"; Fn = gtir }
  { Alias = "gtri"; Fn = gtri }
  { Alias = "gtrr"; Fn = gtrr }
  { Alias = "eqir"; Fn = eqir }
  { Alias = "eqri"; Fn = eqri }
  { Alias = "eqrr"; Fn = eqrr } ]

let instructionPointer = 0

let sampleProgram = [
  "seti 5 0 1"
  "seti 6 0 2"
  "addi 0 1 0"
  "addr 1 2 3"
  "setr 1 0 0"
  "seti 8 0 4"
  "seti 9 0 5"]

let actualProgram = [
  "addi 5 16 5"
  "seti 1 8 4"
  "seti 1 5 3"
  "mulr 4 3 1"
  "eqrr 1 2 1"
  "addr 1 5 5"
  "addi 5 1 5"
  "addr 4 0 0"
  "addi 3 1 3"
  "gtrr 3 2 1"
  "addr 5 1 5"
  "seti 2 5 5"
  "addi 4 1 4"
  "gtrr 4 2 1"
  "addr 1 5 5"
  "seti 1 2 5"
  "mulr 5 5 5"
  "addi 2 2 2"
  "mulr 2 2 2"
  "mulr 5 2 2"
  "muli 2 11 2"
  "addi 1 8 1"
  "mulr 1 5 1"
  "addi 1 18 1"
  "addr 2 1 2"
  "addr 5 0 5"
  "seti 0 7 5"
  "setr 5 0 1"
  "mulr 1 5 1"
  "addr 5 1 1"
  "mulr 5 1 1"
  "muli 1 14 1"
  "mulr 1 5 1"
  "addr 2 1 2"
  "seti 0 0 0"
  "seti 0 9 5" ]



let instRegex = new Regex("^([a-z]+) (\d+) (\d+) (\d+)$")

let parseInput input =
  let rmatch = instRegex.Match(input)
  let instr = rmatch.Groups.[1].Value
  let values = [99L; (rmatch.Groups.[2].Value |> int64); (rmatch.Groups.[3].Value |> int64); (rmatch.Groups.[4].Value |> int64) ]
  { Name = instr
    Fn = (List.find (fun op -> op.Alias = instr) ops).Fn
    Input = values }

let instructionsArray =
  actualProgram
  |> Seq.map (fun str -> parseInput str)
  |> Array.ofSeq


let rec run (input:Instruction []) ip (regs: int64 list) iteration  =
  let nextInputPtr = regs.[ip]
  let instruction = input.[nextInputPtr |> int]
  printfn  "instruction at %i" nextInputPtr
  let output = instruction.Fn regs instruction.Input
  printfn "output %A" output
  if output.[ip] > ((input.Length - 1) |> int64) || iteration > 100 then
    printfn "bailing out early because tried pointing to instruction %A" output.[ip]
    output
  else
    let incrementedPointer = output |> Array.ofList |> (fun arr -> (arr.[ip] <- arr.[ip] + 1L); arr) |> Array.toList
    if incrementedPointer.[ip] > (input.Length - 1 |> int64) then
      printfn "bailing out early because tried pointing to instruction %A" incrementedPointer.[ip]
      incrementedPointer
    else
      if iteration % 10000000 = 0 then
        printfn "iteration %i reg0: %i" iteration regs.[0]
      else ()
      run input ip incrementedPointer (iteration + 1)



let answer =
  run instructionsArray 5 [1L; 0L; 0L; 0L; 0L; 0L] 0
