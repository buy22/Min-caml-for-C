(*AT&T Assembly Language 
  format: 
   lable: Instructor Operator(s)
   [operator1: dest; operator2: src]
   --must use "l""b""w" to identify the type of operators.
*)

open Core
open Stdlib

let att_print s ctx = output_string ctx s

(* usually label for main *)
let global_label f =
  "\t.globl " ^ f ^ "\n" |> att_print

(* label for function *)
let label f =
  f ^ ":\n" |> att_print

(* Instructor without operator *)
let no_op c =
  String.concat ["\t";  c;  "\n"] |> att_print

(* Instructor with 1 operator *)
let one_op c a =
  String.concat ["\t"; c; "\t"; a; "\n"] |> att_print

(* Instructor with 2 operator *)
let two_op c a b =
  String.concat ["\t"; c; "\t"; a; ", "; b; "\n"] |> att_print

let movl = two_op "MOVL"

let movb = two_op "MOVB"

let movw = two_op "MOVW"

let push = one_op "PUSH"

let pop = one_op "POP"

let sete = one_op "SETE"

let setne = one_op "SETNE"

let setl = one_op "SETL"

let setle = one_op "SETLE"

let setg = one_op "SETG"

let setge = one_op "SETGE"

(* Calculation *)
let add = two_op "ADD"

let sub = two_op "SUB"

let nec = one_op "NEC"

let neg = one_op "NEG"

let cmpl = two_op "CMPL"

let mul = two_op "MUL"

let imul = two_op "IMUL"

let div = two_op "DIV"

let idiv = one_op "IDIV"

(* Logic *)
(* "and" & "or" - modify spelling for Ocaml has "and" & "or" *)
let anda = two_op "AND"

let ora = two_op "OR"

let xor = two_op "XOR"

let not = one_op "NOT"

(* Transfer *)
let jcc = one_op "JCC"

let je = one_op "JE"

let jmp = one_op "JMP"

let shl = one_op "SHL"

let shr = one_op "SHR"

let sal = two_op "SAL"

let sar = two_op "SAR"

let rol = two_op "ROL"

let ror = two_op "ROR"

let rcl = two_op "RCL"

let rcr = two_op "RCR"

(* Other items *)
let call = one_op "CALL"

let nop = no_op "NOP"

let ret = no_op "RET"

let enter = two_op "ENTER"

let leave = no_op "LEAVE"

let test = two_op "TEST"