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
  | _ -> raise (Except ("Unknown register r" ^ string_of_int i))

let str_to_col =
  let colors = Hashtbl.create 11 in
  List.iter
    (fun (s, l) -> Hashtbl.add colors s l)
    [
      ("black", Black);
      ("red", Red);
      ("green", Green);
      ("yellow", Yellow);
      ("blue", Blue);
      ("magenta", Magenta);
      ("cyan", Cyan);
      ("white", White);
      ("brightblack", BrightBlack);
      ("brightred", BrightRed);
      ("brightgreen", BrightGreen);
      ("brightyellow", BrightYellow);
      ("brightblue", BrightBlue);
      ("brightmagenta", BrightMagenta);
      ("brightcyan", BrightCyan);
      ("brightwhite", BrightWhite);
    ];
  fun s -> Hashtbl.find_opt colors s
