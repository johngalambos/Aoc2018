// contains2 Contains3
// string -> (bool, bool)

// dupsOfLetter
// string -> int -> bool

// group strings into groups of chars with counts

let countLetters (input:string) = 
  input
  |> Seq.groupBy (fun l -> l)
  |> Seq.map(fun (i, listed) -> (i, Seq.length listed))


let hasN n input = 
  countLetters input
  |> Seq.exists (fun (_, count) -> count = n)
  

let folder (sum2, sum3) x =
  let has2 = hasN 2 x
  let has3 = hasN 3 x
  match (has2, has3) with 
  | (true, true)  -> (sum2 + 1, sum3 + 1)
  | (true, false) -> (sum2 + 1, sum3)
  | (false, true) -> (sum2, sum3 + 1)
  | (false, false) -> (sum2, sum3)

let checksum ids =
  let (sum2, sum3) = List.fold folder (0, 0) ids
  sum2 * sum3

let test = ["abcdef"; "bababc"; "abbcde"; "abcccd"; "aabcdd"; "abcdee"; "ababab"]


let lines = System.IO.File.ReadAllLines(@"C:\Users\john\code\Aoc2018\Day2\input1.txt")
checksum (lines |> List.ofArray)

// for two strings every character is the same except for one
// remove the nth char from each string. If they are identical this must be the one
// there should only be two matching strings for the correct combination

// removeN :: string -> string

let removeN n (x: string) = 
  match n with 
  | 0  -> x.[1..] 
  | _ when n + 1 = String.length x -> x.[..n - 1]
  | _ -> x.[..n - 1] + x.[n + 1..]
  
  
let test2 = ["abcde"; "fghij"; "klmno"; "pqrst"; "fguij"; "axcye"; "wvxyz"]  

let len = test2.[0].Length

let rec findMatches indexes candidates = 
  match indexes with 
  | [] -> failwith "shouldn't have gotten here"
  | h::t ->  
      let possibleMatches = 
        candidates
        |> List.map (fun x -> removeN h x)
        |> List.groupBy (fun x -> x)
        |> List.filter (fun (_, xs) -> List.length xs = 2)

      printfn "%A" possibleMatches
      match possibleMatches with
      | [] -> findMatches t candidates
      | _ -> fst possibleMatches.[0]
      
     


let findIds (candidates:string list) = 
  let len = candidates.[0].Length
  let indexes = [0..len-1]
  printfn "%A" indexes
  findMatches indexes candidates


  
  
