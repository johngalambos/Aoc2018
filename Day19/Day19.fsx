open System.Text.RegularExpressions

type Op =
  { Alias: string
    Fn: int list -> int list -> int list }

type Instruction =
  { Name: string
    Fn: int list -> int list -> int list
    Input: int list }

let addr reg ([op; i1; i2; out]: int list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] + reg.[i2 |> int]
  Array.toList regArr

let addi reg ([op; i1; i2; out]: int list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] + i2
  Array.toList regArr

let mulr reg ([op; i1; i2; out] : int list)=
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] * reg.[i2 |> int]
  Array.toList regArr

let muli reg ([op; i1; i2; out]: int list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] * i2
  Array.toList regArr

let banr reg ([op; i1; i2; out]: int list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] &&& reg.[i2 |> int]
  Array.toList regArr

let bani reg ([op; i1; i2; out]: int list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] &&& i2
  Array.toList regArr

let borr reg ([op; i1; i2; out]: int list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] ||| reg.[i2 |> int]
  Array.toList regArr

let bori reg ([op; i1; i2; out]: int list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int] ||| i2
  Array.toList regArr

let setr reg ([op; i1; i2; out]: int list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- reg.[i1 |> int]
  Array.toList regArr

let seti reg ([op; i1; i2; out]: int list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- i1
  Array.toList regArr

let gtir reg ([op; i1; i2; out]: int list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- if i1 > reg.[i2 |> int] then 1 else 0
  Array.toList regArr

let gtri reg ([op; i1; i2; out]: int list) =
  let regArr = List.toArray reg
  regArr.[out |> int] <- if reg.[i1 |> int] > i2 then 1 else 0
  Array.toList regArr

let gtrr reg ([op; i1; i2; out] : int list)=
  let regArr = List.toArray reg
  regArr.[out |> int] <- if reg.[i1 |> int] > reg.[i2 |> int] then 1 else 0
  Array.toList regArr

let eqir reg ([op; i1; i2; out] : int list)=
  let regArr = List.toArray reg
  regArr.[out |> int] <- if i1 = reg.[i2 |> int] then 1 else 0
  Array.toList regArr

let eqri reg ([op; i1; i2; out] : int list)=
  let regArr = List.toArray reg
  regArr.[out |> int] <- if reg.[i1 |> int] = i2 then 1 else 0
  Array.toList regArr

let eqrr reg ([op; i1; i2; out] : int list)=
  let regArr = List.toArray reg
  regArr.[out |> int] <- if reg.[i1 |> int] = reg.[i2 |> int] then 1 else 0
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

(* the loop as imperative
A <- 0
B <- (10551430 essentially 1)
C <- 10551430
D <- 1
E <- 1

while true
  B <- E * D (this is the sum of the factors of C)
  if B = C then
    B <- 1
    A <- E + A
  else
    B <- 0
  D <- D + 1 //increment D on every loop
  if D > C then
    B <- 1
    E <- E + 1 //incremnt E only when D > C
    if E > C then
      B <- 1
      failwith "we're done"
    else
      B <- 0
      D <- 1 //reset D to one
  else
    B <- 0

*)
let actualProgram = [
  "addi 5 16 5"   //0   Go to instruction 17
  "seti 1 8 4"    //1   E <- 1 //set E to 1
  "seti 1 5 3"    //2   D <- 1 //set D to 1
  "mulr 4 3 1"    //3   B <- E * D //Start of Part 2 loops
  "eqrr 1 2 1"    //4   if B = C then B <- 1 else B <- 0
  "addr 1 5 5"    //5   F <- B + F
  "addi 5 1 5"    //6   F <- F + 1
  "addr 4 0 0"    //7   A <- E + A //for terimation E here will be 10551430 while A will be ? .. we only e
  "addi 3 1 3"    //8   D <- D + 1 //for termination D here will be 10551430
  "gtrr 3 2 1"    //9   if D > C then B <- 1 else B <- 0 for termination D here will be 10551431
  "addr 5 1 5"    //10  F <- F + B
  "seti 2 5 5"    //11  F <- 2
  "addi 4 1 4"    //12  E <- E + 1 //for termination E here will be 10551430
  "gtrr 4 2 1"    //13  if E > C then B <- 1 else B <- 0 //to trigger > than E  must be 10551431
  "addr 1 5 5"    //14  F <- B + F (14,15,16) once E is greater than C go to 16
  "seti 1 2 5"    //15  F <- 1
  "mulr 5 5 5"    //16  F <- F * F (will have value 16 * 16 and the program will quit. What is the value of A? when this happens?)
  "addi 2 2 2"    //17  Start of first program -> sets the value of C for part 2
  "mulr 2 2 2"    //18
  "mulr 5 2 2"    //19
  "muli 2 11 2"   //20
  "addi 1 8 1"    //21
  "mulr 1 5 1"    //22
  "addi 1 18 1"   //23
  "addr 2 1 2"    //24
  "addr 5 0 5"    //25
  "seti 0 7 5"    //26
  "setr 5 0 1"    //27
  "mulr 1 5 1"    //28
  "addr 5 1 1"    //29
  "mulr 5 1 1"    //30
  "muli 1 14 1"   //31
  "mulr 1 5 1"    //32
  "addr 2 1 2"    //33
  "seti 0 0 0"    //34
  "seti 0 9 5"]   //35



let instRegex = new Regex("^([a-z]+) (\d+) (\d+) (\d+)$")

let parseInput input =
  let rmatch = instRegex.Match(input)
  let instr = rmatch.Groups.[1].Value
  let values = [99; (rmatch.Groups.[2].Value |> int); (rmatch.Groups.[3].Value |> int); (rmatch.Groups.[4].Value |> int) ]
  { Name = instr
    Fn = (List.find (fun op -> op.Alias = instr) ops).Fn
    Input = values }

let instructionsArray =
  actualProgram
  |> Seq.map (fun str -> parseInput str)
  |> Array.ofSeq


let rec run (input:Instruction []) ip (regs: int list) iteration  =
  let nextInputPtr = regs.[ip]
  let instruction = input.[nextInputPtr]
  let output = instruction.Fn regs instruction.Input
  // printfn "output %A" output
  if output.[ip] > ((input.Length - 1)) || iteration > 100 then
    printfn "bailing out early because tried pointing to instruction %A" output.[ip]
    output
  else
    let incrementedPointer = output |> Array.ofList |> (fun arr -> (arr.[ip] <- arr.[ip] + 1); arr) |> Array.toList
    if incrementedPointer.[ip] > (input.Length - 1) then
      printfn "bailing out early because tried pointing to instruction %A" incrementedPointer.[ip]
      incrementedPointer
    else
      if iteration % 10000000 = 0 then
        printfn "iteration %i reg0: %i" iteration regs.[0]
      else ()
      printfn "(%i, %s) in:%A out:%A" nextInputPtr instruction.Name regs incrementedPointer
      run input ip incrementedPointer (iteration + 1)



let answer =
  run instructionsArray 5 [1; 0; 0; 0; 0; 0] 0
  //the first teim we hit the constraint
  // run instructionsArray 5 [0; 10551430; 10551430; 10551430; 1; 3] 0
  //the second time we hit the constraint
  // run instructionsArray 5 [1; 10551430; 10551430; 10551430; 2; 3] 0
  // run instructionsArray 5 [0; 10551430; 10551430; 10551430; 1; 3] 0

let n = 10551430

let factors =
  [ 1..(10551430)]
  |> Seq.filter (fun i -> 10551430 % i = 0)
// I guessed 18992592 which was rong
let answer =
  factors
  |> Seq.sum
// factors
// |> Seq.sum
// 18992592 is the sum of the factors
