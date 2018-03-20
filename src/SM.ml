open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

let insn_to_str i = match i with
  | BINOP op -> "binop " ^ op
  | CONST i -> "const " ^ string_of_int i
  | READ -> "read"
  | WRITE -> "write"
  | LD v -> "ld " ^ v
  | ST v -> "st " ^ v

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config


let to_int b = if b then 1 else 0
let to_bool num = num != 0

let eval_binop lVal rVal binop : int = match binop with
  | "+" -> lVal + rVal
  | "-" -> lVal - rVal
  | "*" -> lVal * rVal
  | "/" -> lVal / rVal
  | "%" -> lVal mod rVal
  | "==" -> to_int (lVal == rVal)
  | "!=" -> to_int (lVal != rVal)
  | "<=" -> to_int (lVal <= rVal)
  | "<" -> to_int (lVal < rVal)
  | ">" -> to_int (lVal > rVal)
  | ">=" -> to_int (lVal >= rVal)
  | "!!" -> to_int (to_bool lVal || to_bool rVal)
  | "&&" -> to_int (to_bool lVal && to_bool rVal)
  | _ -> failwith "Operator not supported "

let eval_insn conf ins: config = match ins, conf with
  | BINOP op, ((y::x::stack_tail), stmt_config) -> (((eval_binop x y op) :: stack_tail), stmt_config)
  | CONST c, (stack_tail, stmt_config) -> c :: stack_tail, stmt_config
  | READ, (stack_tail, (expr_state, z::istream, ostream)) -> z :: stack_tail, (expr_state, istream, ostream)
  | WRITE, (z::stack_tail, (expr_state, istream, ostream)) -> stack_tail, (expr_state, istream, ostream @ [z])
  | LD var_name, (stack_tail, (expr_state, istream, ostream)) -> (expr_state var_name)::stack_tail, (expr_state, istream, ostream)
  | ST var_name, (x::stack_tail, (expr_state, istream, ostream)) -> stack_tail, ((Language.Expr.update var_name x expr_state), istream, ostream)
  | _ -> failwith "Bad instruction or configuration"

(* Stack machine interpreter
     val eval : config -> prg -> config
   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval conf prog: config = match prog with
  | ins :: tail -> eval (eval_insn conf ins) tail
  | [] -> conf


(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> expr e @ [WRITE]
  | Stmt.Assign (x, e) -> expr e @ [ST x]
