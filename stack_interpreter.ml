type stackValue = BOOL of bool | INT of int | ERROR | S of string | N of string | UNIT 
type command = ADD | SUB | MUL | DIV | PUSH of stackValue | POP | REM | NEG | SWAP | TOSTRING | PRINTLN | QUIT | CAT | AND | OR | NOT | EQUAL | LESSTHAN | BIND | IF | LET | END
(* add more commands later on *)


(* look at FIRST character of the string since 
+string will start with quotation mark 
+bools will be :true: or :false:
+error will be :error:
+unit will be :unit:
+ints will start with a 0-9 digit or - if negative
+names will start with underscore OR char *)
let string2stackVal (str:string) : stackValue =
  match str with
  | ":true:" -> BOOL true
  | ":false:" -> BOOL false
  | ":error:" -> ERROR
  | ":unit:" -> UNIT
  | _ -> let sub_str = String.sub str 0 1 in
    match sub_str with
    | "\"" -> S (String.sub str 1 (String.length str - 2))
    | "-" -> (* handle negative numbers *)
      ( match String.contains str '.' with
        | false -> let ret = int_of_string(String.sub str 0 (String.length str)) in INT ret
        | true -> ERROR )
    | "0" -> 
      ( match String.contains str '.' with
        | false -> let ret = int_of_string(str) in INT ret 
        | true -> ERROR )
    | "1" -> 
      ( match String.contains str '.' with
        | false -> let ret = int_of_string(str) in INT ret 
        | true -> ERROR )
    | "2" ->       
      ( match String.contains str '.' with
        | false -> let ret = int_of_string(str) in INT ret 
        | true -> ERROR )
    | "3" -> 
      ( match String.contains str '.' with
        | false -> let ret = int_of_string(str) in INT ret 
        | true -> ERROR )
    | "4" -> 
      ( match String.contains str '.' with
      | false -> let ret = int_of_string(str) in INT ret 
      | true -> ERROR )
    | "5" ->
      ( match String.contains str '.' with
        | false -> let ret = int_of_string(str) in INT ret 
        | true -> ERROR )
    | "6" ->
      ( match String.contains str '.' with
        | false -> let ret = int_of_string(str) in INT ret 
        | true -> ERROR )
    | "7" ->
      ( match String.contains str '.' with
        | false -> let ret = int_of_string(str) in INT ret 
        | true -> ERROR )
    | "8" -> 
      ( match String.contains str '.' with
        | false -> let ret = int_of_string(str) in INT ret 
        | true -> ERROR )
    | "9" -> 
      ( match String.contains str '.' with
        | false -> let ret = int_of_string(str) in INT ret 
        | true -> ERROR )
    | "_" -> N str
    | _ -> N str


let string2command (str:string) : command =
  match str with 
  | "add" -> ADD
  | "sub" -> SUB
  | "mul" -> MUL
  | "div" -> DIV
  | "pop" -> POP
  | "rem" -> REM
  | "neg" -> NEG
  | "swap" -> SWAP
  | "toString" -> TOSTRING
  | "println" -> PRINTLN
  | "quit" -> QUIT
  | "cat" -> CAT
  | "and" -> AND
  | "or" -> OR
  | "not" -> NOT
  | "equal" -> EQUAL
  | "lessThan" -> LESSTHAN
  | "bind" -> BIND
  | "if" -> IF
  | "let" -> LET
  | "end" -> END
  | _ -> let sub_str = String.sub str 0 4 in
    match sub_str with 
    | "push" -> PUSH (string2stackVal (String.sub str 5 ((String.length str) - 5)))
    | _ -> PUSH ERROR

let rec nameFind (name:string) (memory : (string*stackValue) list list)  : stackValue = 
  match memory with
  |(mem::more) -> let n = (try List.assoc name mem with Not_found -> (nameFind name more)) in n
  | _ -> ERROR

let rec stackVal2string sc : string =
  match sc with
    | BOOL x -> ":" ^ string_of_bool x ^ ":"
    | INT x -> string_of_int x
    | UNIT -> ":unit:"
    | ERROR  -> ":error:"
    | S string -> string 
    | N string -> string 

  let rec stackVal2stringNEWLINE sc =
    match sc with
    | BOOL x -> ":" ^ string_of_bool x ^ ":\n"
    | INT x -> string_of_int x ^ "\n"
    | UNIT -> ":unit:\n"
    | ERROR  -> ":error:\n"
    | S string -> string ^ "\n"
    | N string -> string ^ "\n"

let int_equal a b : bool =
  match a-b with
  | 0 -> true
  | _ -> false

let int_less a b : bool =
  let minus = a-b in 
  match minus > 0 with
  | true -> true
  | false -> false

let rec write_list_to_oc oc (strings : string list) : unit =
  match strings with
  | [] -> ()
  | smth::[] -> (Printf.fprintf oc "%s\n" smth)
  | smth::moreStack -> (Printf.fprintf oc "%s\n" smth); write_list_to_oc oc moreStack

let nameAsInt name mem : stackValue = 
  let sv = nameFind name mem in 
  match sv with 
  | INT(v) -> INT(v)
  | _ -> ERROR

let nameAsBool name mem : stackValue =
  let sv = nameFind name mem in
  match sv with
  | BOOL(v) -> BOOL(v)
  | _ -> ERROR

let interpreter ((input:string),(output:string)) : unit = 

  let ic = open_in input 
  in
  let oc = open_out output 
  in

  (* reads all commands given in the input file *)
  let rec loop_read acc = 
    try
      let l = String.trim(input_line ic) in loop_read (l::acc)
      (* String.trim removes unnecessary whitespace from the input *)
    with
      | End_of_file -> List.rev acc
  in

  let strList : string list = loop_read [] 
  in

  let commandList = List.map string2command strList
  in

  let stack : stackValue list list = [[]]
  in

  let mem : (string*stackValue) list list = [[]]
  in

  let rec processor (commandList: command list) (stack: stackValue list list)  (mem: ((string*stackValue) list ) list) : unit =
    (* iterate thrugh command list and build up stack *)
    (*match (commandList) with
    | [] -> print_string "done"
    | comm::moreComms -> let b = print_string (command2string comm) in b
    ;*)
    
    match (commandList,stack, mem) with 
    (* PART 1 *)
    | (QUIT::_, s, m) -> close_in ic; close_out oc
    | (comms, [], m) -> processor comms ([]::[]) m (* catch case to fix pattern match missing case *)
    | (comms, s, []) -> processor comms (s) ([]::[]) (* catch case to fix pattern match missing case *)
    | (PUSH INT(a)::moreComms, s::more, m) -> processor moreComms ( (INT(a)::s)::more ) m
    | (PUSH S(a)::moreComms, s::more, m) -> processor moreComms ( (S(a)::s)::more ) m 
    | (PUSH N(a)::moreComms, s::more, m) -> processor moreComms ( (N(a)::s)::more ) m
    | (PUSH BOOL(a)::moreComms, s::more, m) -> processor moreComms ( (BOOL(a)::s)::more ) m 
    | (PUSH ERROR::moreComms, s::more, m) -> processor moreComms ( (ERROR::s)::more ) m
    | (PUSH UNIT::moreComms, s::more, m) -> processor moreComms ( (UNIT::s)::more ) m
    | (TOSTRING::moreComms, []::more, m) -> processor moreComms ((ERROR::[])::more) m 
    | (TOSTRING::moreComms, (a::s)::moreStack, m) -> let b = (stackVal2string a) in processor moreComms ( (S(b)::s)::moreStack) m
    | (PRINTLN::moreComms, []::more, m) -> processor moreComms ((ERROR::[])::more) m
    | (PRINTLN::moreComms, (S(s)::moreStack)::more, m) -> (Printf.fprintf oc "%s\n" s); processor moreComms (moreStack::more) m
    | (PRINTLN::moreComms, (n::moreStack)::more, m) -> processor moreComms ((ERROR::n::moreStack)::more) m
    (* updated ADD to access name's value *)
    | (ADD::moreComms, (INT(a)::INT(b)::moreStack)::more, m) -> processor moreComms ( (INT(a+b)::moreStack)::more ) m 
    | (ADD::moreComms, (N(n1)::INT(b)::moreStack)::more, m) -> let a = (nameAsInt n1 mem) in
      ( match a with
        | INT(ai) -> processor moreComms ( (INT(ai+b)::moreStack)::more ) mem 
        | _ -> processor moreComms ((ERROR::N(n1)::INT(b)::moreStack)::more) mem)
    | (ADD::moreComms, (INT(a)::N(n2)::moreStack)::more, m) -> let b = (nameAsInt n2 mem) in
      ( match b with
        | INT(bi) -> processor moreComms ( (INT(a+bi)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::INT(a)::N(n2)::moreStack)::more) m)
    | (ADD::moreComms, (N(n1)::N(n2)::moreStack)::more, m) -> let a = (nameAsInt n1 mem) in let b = (nameAsInt n2 mem) in 
      ( match a, b with
        | INT(ai), INT(bi) -> processor moreComms ( (INT(ai+bi)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::N(n2)::moreStack)::more) m)
    | (ADD::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    (**)
    (* updated SUB to access name's value *)
    | (SUB::moreComms, (INT(a)::INT(b)::moreStack)::more, m) -> processor moreComms ( (INT(b-a)::moreStack)::more ) m 
    | (SUB::moreComms, (N(n1)::INT(b)::moreStack)::more, m) -> let a = (nameAsInt n1 mem) in
      ( match a with
        | INT(ai) -> processor moreComms ( (INT(b-ai)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::INT(b)::moreStack)::more) m)
    | (SUB::moreComms, (INT(a)::N(n2)::moreStack)::more, m) -> let b = (nameAsInt n2 mem) in
      ( match b with
        | INT(bi) -> processor moreComms ( (INT(bi-a)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::INT(a)::N(n2)::moreStack)::more) m)
    | (SUB::moreComms, (N(n1)::N(n2)::moreStack)::more, m) -> let a = (nameAsInt n1 mem) in let b = (nameAsInt n2 mem) in 
      ( match a, b with
        | INT(ai), INT(bi) -> processor moreComms ( (INT(bi-ai)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::N(n2)::moreStack)::more) m)
    | (SUB::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    (**)
    (* updated MUL to access name's value *)
    | (MUL::moreComms, (INT(a)::INT(b)::moreStack)::more, m) -> processor moreComms ( (INT(a*b)::moreStack)::more ) m 
    | (MUL::moreComms, (N(n1)::INT(b)::moreStack)::more, m) -> let a = (nameAsInt n1 mem) in
      ( match a with
        | INT(ai) -> processor moreComms ( (INT(ai*b)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::INT(b)::moreStack)::more) m)
    | (MUL::moreComms, (INT(a)::N(n2)::moreStack)::more, m) -> let b = (nameAsInt n2 mem) in
      ( match b with
        | INT(bi) -> processor moreComms ( (INT(a*bi)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::INT(a)::N(n2)::moreStack)::more) m)
    | (MUL::moreComms, (N(n1)::N(n2)::moreStack)::more, m) -> let a = (nameAsInt n1 mem) in let b = (nameAsInt n2 mem) in 
      ( match a, b with
        | INT(ai), INT(bi) -> processor moreComms ( (INT(ai*bi)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::N(n2)::moreStack)::more) m )
    | (MUL::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m 
    (**)
    (* updated DIV to access name's value *)
    | (DIV::moreComms, (INT(0)::moreStack)::more, m) -> processor moreComms ((ERROR::INT(0)::moreStack)::more) m
    | (DIV::moreComms, (INT(a)::INT(b)::moreStack)::more, m) -> processor moreComms ( (INT(b/a)::moreStack)::more ) m 
    | (DIV::moreComms, (N(n1)::INT(b)::moreStack)::more, m) -> let a = (nameAsInt n1 mem) in
      (match a with 
        | INT(0) -> processor moreComms ((ERROR::N(n1)::INT(b)::moreStack)::more) m
        | INT(ai) -> processor moreComms ((INT(b/ai)::moreStack)::more) m
        | _ -> processor moreComms ((ERROR::N(n1)::INT(b)::moreStack)::more) m )
    | (DIV::moreComms, (INT(a)::N(n2)::moreStack)::more, m) -> let b = (nameAsInt n2 mem) in
      (match b with 
        | INT(bi) -> processor moreComms ((INT(bi/a)::moreStack)::more) m 
        | _ -> processor moreComms ((ERROR::INT(a)::N(n2)::moreStack)::more) m )
    | (DIV::moreComms, (N(n1)::N(n2)::moreStack)::more, m) -> let a = (nameAsInt n1 mem) in let b = (nameAsInt n2 mem) in
      (match a,b with
        | INT(0), INT(bi) -> processor moreComms ((ERROR::N(n1)::N(n2)::moreStack)::more) m
        | INT(ai), INT(bi) -> processor moreComms ( (INT(bi/ai)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::N(n2)::moreStack)::more) m )
    | (DIV::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    (**)
    | (POP::moreComms, []::more, m) -> processor moreComms ((ERROR::[])::more) m
    | (POP::moreComms, (popping::moreStack)::more, m) -> processor moreComms (moreStack::more) m 
    | (REM::moreComms, (INT(0)::INT(b)::moreStack)::more, m) -> processor moreComms ((ERROR::INT(0)::INT(b)::moreStack)::more) m
    | (REM::moreComms, (INT(0)::N(b)::moreStack)::more, m) -> processor moreComms ((ERROR::INT(0)::N(b)::moreStack)::more) m
    | (REM::moreComms, (INT(a)::INT(b)::moreStack)::more, m) -> processor moreComms ( (INT(b mod a)::moreStack)::more ) m 
    (* updated REM to access name's value *)
    | (REM::moreComms, (N(n1)::INT(b)::moreStack)::more, m) -> let a = (nameAsInt n1 mem) in
      (match a with 
        | INT(0) -> processor moreComms ((ERROR::N(n1)::INT(b)::moreStack)::more) m
        | INT(ai) -> processor moreComms ((INT(b mod ai)::moreStack)::more) m
        | _ -> processor moreComms ((ERROR::N(n1)::INT(b)::moreStack)::more) m )
    | (REM::moreComms, (INT(a)::N(n2)::moreStack)::more, m) -> let b = (nameAsInt n2 mem) in
      (match b with 
        | INT(bi) -> processor moreComms ((INT(bi mod a)::moreStack)::more) m 
        | _ -> processor moreComms ((ERROR::INT(a)::N(n2)::moreStack)::more) m )
    | (REM::moreComms, (N(n1)::N(n2)::moreStack)::more, m) -> let a = (nameAsInt n1 mem) in let b = (nameAsInt n2 mem) in
      (match a,b with
        | INT(0), INT(bi) -> processor moreComms ((ERROR::N(n1)::N(n2)::moreStack)::more) m
        | INT(ai), INT(bi) -> processor moreComms ( (INT(bi mod ai)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::N(n2)::moreStack)::more) m )
    | (REM::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    (**)
    | (SWAP::moreComms, (a::b::moreStack)::more, m) -> processor moreComms ( (b::a::moreStack)::more ) m 
    | (SWAP::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    | (NEG::moreComms, []::more, m) -> processor moreComms ((ERROR::[])::more) m
    | (NEG::moreComms, (INT(a)::moreStack)::more, m) -> processor moreComms ((INT(0-a)::moreStack)::more) m (* updated to access name's value *)
    | (NEG::moreComms, (N(n1)::moreStack)::more, m) -> let a = (nameAsInt n1 mem) in
      ( match a with
        | INT(ai) -> processor moreComms ( (INT(0-ai)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::moreStack)::more) m)
    | (NEG::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    
    (* PART 2 *)

    (* updated CAT to access name's value *)
    | (CAT::moreComms, (S(a)::S(b)::moreStack)::more, m) -> processor moreComms ((S(b^a)::moreStack)::more) m
    | (CAT::moreComms, (N(a)::S(b)::moreStack)::more, m) -> 
        (match nameFind a m with
          | S(ai) -> processor moreComms ((S(b^ai)::moreStack)::more) m
          | _ -> processor moreComms ((ERROR::N(a)::S(b)::moreStack)::more) m )
    | (CAT::moreComms, (S(a)::N(b)::moreStack)::more, m) -> 
      (match nameFind b m with
        | S(bi) -> processor moreComms ((S(bi^a)::moreStack)::more) m
        | _ -> processor moreComms ((ERROR::S(a)::N(b)::moreStack)::more) m ) 
    | (CAT::moreComms, (N(a)::N(b)::moreStack)::more, m) ->
      (match (nameFind a m), (nameFind b m) with
        | S(ai),S(bi) -> processor moreComms ((S(bi^ai)::moreStack)::more) m
        | _ -> processor moreComms ((ERROR::N(a)::N(b)::moreStack)::more) m )
    | (CAT::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    (**) (* START HERE!!!!!!!!!!*)
    (* updated AND to access name's value *)
    | (AND::moreComms, (BOOL(a)::BOOL(b)::moreStack)::more, m) -> processor moreComms ((BOOL(a&&b)::moreStack)::more) m 
    | (AND::moreComms, (N(n1)::BOOL(b)::moreStack)::more, m) -> let a = (nameAsBool n1 mem) in
      ( match a with
        | BOOL(ai) -> processor moreComms ( (BOOL(ai&&b)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::BOOL(b)::moreStack)::more) m )
    | (AND::moreComms, (BOOL(a)::N(n2)::moreStack)::more, m) -> let b = (nameAsBool n2 mem) in
      ( match b with
        | BOOL(bi) -> processor moreComms ( (BOOL(a&&bi)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::BOOL(a)::N(n2)::moreStack)::more) m)
    | (AND::moreComms, (N(n1)::N(n2)::moreStack)::more, m) -> let a = (nameAsBool n1 mem) in let b = (nameAsBool n2 mem) in 
      ( match a, b with
        | BOOL(ai), BOOL(bi) -> processor moreComms ( (BOOL(ai&&bi)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::N(n2)::moreStack)::more) m )
    | (AND::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m 
    (**)
    (* updated OR to access name's value *)
    | (OR::moreComms, (BOOL(a)::BOOL(b)::moreStack)::more, m) -> processor moreComms ((BOOL(a||b)::moreStack)::more) m 
    | (OR::moreComms, (N(n1)::BOOL(b)::moreStack)::more, m) -> let a = (nameAsBool n1 mem) in
      ( match a with
        | BOOL(ai) -> processor moreComms ( (BOOL(ai||b)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::BOOL(b)::moreStack)::more) m )
    | (OR::moreComms, (BOOL(a)::N(n2)::moreStack)::more, m) -> let b = (nameAsBool n2 mem) in
      ( match b with
        | BOOL(bi) -> processor moreComms ( (BOOL(a||bi)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::BOOL(a)::N(n2)::moreStack)::more) m)
    | (OR::moreComms, (N(n1)::N(n2)::moreStack)::more, m) -> let a = (nameAsBool n1 mem) in let b = (nameAsBool n2 mem) in 
      ( match a, b with
        | BOOL(ai), BOOL(bi) -> processor moreComms ( (BOOL(ai||bi)::moreStack)::more ) m 
        | _ -> processor moreComms ((ERROR::N(n1)::N(n2)::moreStack)::more) m )
    | (OR::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m 
    (**)
    | (NOT::moreComms, (BOOL(true)::moreStack)::more, m) -> processor moreComms ((BOOL(false)::moreStack)::more) m 
    | (NOT::moreComms, (BOOL(false)::moreStack)::more, m) -> processor moreComms ((BOOL(true)::moreStack)::more) m (* updated to access name's value *)
    | (NOT::moreComms, (N(n1)::moreStack)::more, m) -> let a = (nameAsBool n1 mem) in
      ( match a with
        | BOOL(true) -> processor moreComms ((BOOL(false)::moreStack)::more) m 
        | BOOL(false) -> processor moreComms ((BOOL(true)::moreStack)::more) m
        | _ -> processor moreComms ((ERROR::N(n1)::moreStack)::more) m)
    | (NOT::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    (* updated EQUAL to access name's value *)
    | (EQUAL::moreComms, (INT(a)::INT(b)::moreStack)::more, m) -> let compared = (int_equal a b) in processor moreComms ((BOOL(compared)::moreStack)::more) m 
    | (EQUAL::moreComms, (N(n1)::INT(b)::moreStack)::more, m) -> 
      ( match (nameFind n1 m) with
      | INT(a) -> let compared = (int_equal a b) in processor moreComms ((BOOL(compared)::moreStack)::more) m 
      | _ -> processor moreComms ((ERROR::N(n1)::INT(b)::moreStack)::more) m )
    | (EQUAL::moreComms, (INT(a)::N(n2)::moreStack)::more, m) -> 
      ( match (nameFind n2 m) with
      | INT(b) -> let compared = (int_equal a b) in processor moreComms ((BOOL(compared)::moreStack)::more) m 
      | _ -> processor moreComms ((ERROR::INT(a)::N(n2)::moreStack)::more) m )
    | (EQUAL::moreComms, (N(n1)::N(n2)::moreStack)::more, m) -> 
      ( match (nameFind n1 m) , (nameFind n2 m) with
      | INT(a), INT(b) -> let compared = (int_equal a b) in processor moreComms ((BOOL(compared)::moreStack)::more) m 
      | _ -> processor moreComms ((ERROR::N(n1)::N(n2)::moreStack)::more) m )
    | (EQUAL::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    (**)
    (* updated LESSTHAN to access name's value *)
    | (LESSTHAN::moreComms, (INT(a)::INT(b)::moreStack)::more, m) -> let compared = (int_less a b) in processor moreComms ((BOOL(compared)::moreStack)::more) m 
    | (LESSTHAN::moreComms, (N(n1)::INT(b)::moreStack)::more, m) -> 
      ( match (nameFind n1 m) with
      | INT(a) -> let compared = (int_less a b) in processor moreComms ((BOOL(compared)::moreStack)::more) m 
      | _ -> processor moreComms ((ERROR::N(n1)::INT(b)::moreStack)::more) m )
    | (LESSTHAN::moreComms, (INT(a)::N(n2)::moreStack)::more, m) -> 
      ( match (nameFind n2 m) with
      | INT(b) -> let compared = (int_less a b) in processor moreComms ((BOOL(compared)::moreStack)::more) m 
      | _ -> processor moreComms ((ERROR::INT(a)::N(n2)::moreStack)::more) m ) 
    | (LESSTHAN::moreComms, (N(n1)::N(n2)::moreStack)::more, m) -> 
      ( match (nameFind n1 m) , (nameFind n2 m) with
      | INT(a), INT(b) -> let compared = (int_less a b) in processor moreComms ((BOOL(compared)::moreStack)::more) m 
      | _ -> processor moreComms ((ERROR::N(n1)::N(n2)::moreStack)::more) m )
    | (LESSTHAN::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    (**)
    | (BIND::moreComms, (ERROR::N(name)::moreStack)::more, m) -> processor moreComms ((ERROR::ERROR::N(name)::moreStack)::more) m
    
    | (BIND::moreComms, (N(value)::N(name)::moreStack)::more, m::moreMem) -> let found = (nameFind value mem) in 
      ( match found with 
      | ERROR -> processor moreComms ((ERROR::N(value)::N(name)::moreStack)::more) (m::moreMem)
      | _ -> processor moreComms ((UNIT::moreStack)::more) (((name, found)::m)::moreMem) )
    (* !!!!!!!!! may be doing the wrong thing if name does not exist ^above case !!!!!!!! *)
    | (BIND::moreComms, (value::N(name)::moreStack)::more, m::moreMem) -> processor moreComms ((UNIT::moreStack)::more) (((name , value)::m)::moreMem)
    | (BIND::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    (* updated IF to access name's value *)
    | (IF::moreComms, (a::b::BOOL(true)::moreStack)::more, m) -> processor moreComms ((a::moreStack)::more) m
    | (IF::moreComms, (a::b::BOOL(false)::moreStack)::more, m) -> processor moreComms ((b::moreStack)::more) m 
    | (IF::moreComms, (a::b::N(n1)::moreStack)::more, m) -> let z = nameFind n1 m in 
      (match z with 
        | BOOL(true) -> processor moreComms ((a::moreStack)::more) m
        | BOOL(false) -> processor moreComms ((b::moreStack)::more) m
        | _ -> processor moreComms ((ERROR::a::b::N(n1)::moreStack)::more) m )
    | (IF::moreComms, s::more, m) -> processor moreComms ((ERROR::s)::more) m
    (**)
    | (LET::moreComms, s, m) -> processor moreComms ([]::s) ([]::m)
    | (END::moreComms, (returning::moreStack)::(newTop)::more, m::moreMem) -> processor moreComms ((returning::newTop)::more) moreMem
    | (END::moreComms, []::(newTop)::more, m::moreMem) -> processor moreComms ((ERROR::newTop)::more) moreMem
    (*| (END::moreComms, s::more::[], m) -> processor moreComms ([ERROR::more]) m*)
    | (END::moreComms, s::more, m ) -> processor moreComms ([ERROR]::more) m
    (*| (comm::moreComms,_,_) -> print_string "exited\n"; close_in ic; close_out oc *)(* most likely remove after creating other cases *)
    | ([],_,_) -> print_string "remember to quit.\n"
  in
  
  processor commandList stack mem

(*let test = 
  let inFile = "in.txt"
  in
  let outFile = "out"
  in
  interpreter (inFile, outFile) *)
  (*let inFile = "input2.txt"
  in
  let outFile = "output2.txt"
  in
  interpreter (inFile, outFile)*)