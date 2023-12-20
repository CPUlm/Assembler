open Ast

exception Except of string

let int_to_reg i =
  match i with
  | 0 -> R0
  | 1 -> R1
  | 2 -> R2
  | 3 -> R3
  | 4 -> R4
  | 5 -> R5
  | 6 -> R6
  | 7 -> R7
  | 8 -> R8
  | 9 -> R9
  | 10 -> R10
  | 11 -> R11
  | 12 -> R12
  | 13 -> R13
  | 14 -> R14
  | 15 -> R15
  | 16 -> R16
  | 17 -> R17
  | 18 -> R18
  | 19 -> R19
  | 20 -> R20
  | 21 -> R21
  | 22 -> R22
  | 23 -> R23
  | 24 -> R24
  | 25 -> R25
  | 26 -> R26
  | 27 -> R27
  | 28 -> ROut
  | 29 -> SP
  | 30 -> FP
  | 31 -> PrivateReg
  | _ -> raise (Except ("Unknown register : " ^ string_of_int i))

let rec casse_couple l =
  match l with
  | [] -> ([], [])
  | (a, b) :: t ->
      let l1, l2 = casse_couple t in
      let a =
        List.fold_left
          (fun acc (a1, a2) ->
            match (a1, a2) with
            | None, None -> acc
            | a1, Some i -> acc @ [ (a1, i) ]
            | Some _, None -> failwith "A label without instruction ?!")
          [] a
      in
      (a @ l1, b @ l2)

let format_file l =
  let l1, l2 = casse_couple l in
  { text = l1; data = l2 }
