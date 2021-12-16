open Core
open Ast
open Att
open Context

(* helper *)
let (>>) f g a = g (f a)

let cint i =  "$" ^ (string_of_int i)

let off i a =
  String.concat [string_of_int i; "("; a; ")"]

let deallocate_vars ctx1 ctx2 =
  ignore @@ addq (cint (ctx1.index - ctx2.index)) "%rsp" ctx2;
  keep_labelc ctx1 ctx2

(* generate *)
let gen_const c =
  match c with
  | Int i -> movl ("$" ^ string_of_int i) "%eax"
  | Char c -> movb ("$" ^ string_of_int (Char.to_int c)) "%al"
  | String s -> my_print_string s
  | _ -> Fn.id

let gen_compare (inst : string -> context -> context) ctx =
  ctx
  |> cmpl "%ecx" "%eax"
  |> movl "$0" "%eax"
  |> inst "%al"

let gen_binop binop =
  match binop with
  | Add -> add "%ecx" "%eax"
  | Sub -> sub "%ecx" "%eax"
  | Mult -> imul "%ecx" "%eax"
  | Div -> Fn.id
    >> movl "$0" "%eax"
    >> idiv "%ecx"
  | Mod -> Fn.id
    >> movl "$0" "%edx"
    >> idiv "%ecx"
    >> movl "%edx" "%eax"
  | Lt -> gen_compare setl
  | Le -> gen_compare setle
  | Gt -> gen_compare setg
  | Ge -> gen_compare setge
  | Neq -> gen_compare setne
  | Eq -> gen_compare sete
  | And -> Fn.id
    >> cmpl "$0" "%eax"
    >> movl "$0" "%eax"
    >> setne "%al"
    >> cmpl "$0" "%ecx"
    >> movl "$0" "%ecx"
    >> setne "%cl"
    >> anda "%cl" "%al"
  | Or -> Fn.id
    >> ora "%ecx" "%eax"
    >> movl "$0" "%eax"
    >> setne "%al"
  | Xor -> xor "%ecx" "%eax"

let gen_assign_op assignop = 
  match assignop with
  | Equals -> Eq

let gen_monop (monop : monop) ctx =
  match monop with
  | Negate -> neg "%eax" ctx
  | Pos -> ctx
  | Complement -> nec "%eax" ctx
  | Not -> ctx
    |> cmpl "$0" "%eax"
    |> movl "$0" "%eax"
    |> sete "%al"

let rec get_type_size = function
  | IntType -> 4
  | CharType -> 1

let rec get_exp_type ctx = function
  | Const c -> IntType
  | Var id -> (find_var id ctx).var_t
  | MonOp (_ , e) -> get_exp_type ctx e
  | BinOp (_ , e1, e2)
  | TernOp (_, e1, e2) -> get_exp_type ctx e1
  | Assign (_, e, _) -> get_exp_type ctx e
  | FunCall _ -> IntType

let rec gen_exp e (ctx : context) =
  match e with
  | Const c ->
    gen_const c ctx
  | Var id ->
    let v =  find_var id ctx in
      movw (off v.loc "%rbp") "%rax" ctx
  | MonOp (monop, e) -> ctx
    |> gen_exp e
    |> gen_monop monop
  | BinOp (binop, e1, e2) -> ctx
    |> gen_exp e1
    |> push "%rax"
    |> gen_exp e2
    |> movw "%rax" "%rcx"
    |> pop "%rax"
    |> gen_binop binop
  | TernOp (cond, texp, fexp) ->
    let lb0 = get_new_label ~name:"CDA" ctx in
      let lb1 = get_new_label ~name:"CDB" ctx in
        ctx
          |> inc_labelc
          |> gen_exp cond
          |> cmpl "$0" "%eax"
          |> je lb0
          |> gen_exp texp
          |> jmp lb1
          |> label lb0
          |> gen_exp fexp
          |> label lb1
  | Assign (Equals, Var id, rexp) ->
    let v = find_var id ctx in
      ctx
        |> gen_exp rexp
        |> movw "%rax" (off v.loc "%rbp")
  | Assign (Equals, _, rexp) ->
    raise (CodeGenError
              "the left hand side of an assignment should be a variable or a dereference")
  | Assign (op, lexp, rexp) ->
    let bexp = (BinOp (gen_assign_op op, lexp, rexp)) in
      gen_exp (Assign (Equals, lexp, bexp)) ctx
  | FunCall (f, args) -> (* TODO *)
    gen_args 0 args ctx
      |> call f

let gen_declaration (de : declaration) ctx =
  (match get_var_level de.name ctx with
   | Some l ->
     if l = ctx.scope_levelc
     then raise (CodeGenError(de.var_name ^ " has already been defined in the same block"))
     else ()
   | None -> ());
  (match de.init with
   | Some iexp ->
     gen_exp iexp ctx
   | None ->
     movw "$0" "%rax" ctx)
  |> push "%rax"
  |> add_var de.var_name de.var_type

let gen_fun_end = leave >> ret

let gen_statement sta ctx = 
  match sta with
  | Block ss ->
    ctx
    |> inc_scope_level
    |> gen_statements ss
    |> deallocate_vars ctx
  | If ifs ->
    let lb0 = get_new_label ~name:"IFA" ctx in
      let lb1 = get_new_label ~name:"IFB" ctx in
        ctx
        |> inc_labelc
        |> gen_exp ifs.cond
        |> cmpl "$0" "%eax"
        |> je lb0
        |> gen_statement ifs.if_body
        |> jmp lb1
        |> label lb0
        |> (match ifs.else_body with
            | Some fs -> gen_statement fs
            | None -> Fn.id)
        |> label lb1
  | Exp e ->
    gen_exp e ctx
  | For f ->
    gen_for (gen_exp f.init) f.cond f.post f.body ctx
  | ForDecl f ->
    gen_for (gen_decl_exp f.init) f.cond f.post f.body ctx
  | While w ->
    let lb0 = get_new_label ~name:"WHA" ctx in
      let lb1 = get_new_label ~name:"WHB" ctx in
        ctx
        |> inc_labelc
        |> set_labels lb0 lb1
        |> label lb0
        |> set_labels lb0 lb1
        |> gen_exp w.cond
        |> cmpl "$0" "%eax"
        |> je lb1
        |> gen_statement w.body
        |> jmp lb0
        |> label lb1
        |> unset_labels
  | DoWhlie w ->
    let lb0 = get_new_label ~name:"DOA" ctx in
      let lb1 = get_new_label ~name:"DOB" ctx in
        ctx
        |> inc_labelc
        |> set_labels lb0 lb1
        |> label lb0
        |> gen_statement w.body
        |> gen_exp w.cond
        |> cmpl "$0" "%eax"
        |> je lb1
        |> jmp lb0
        |> label lb1
        |> unset_labels
  | ReturnVal e -> ctx
    |> gen_exp e
    |> gen_fun_end
  | Break ->
    (match ctx.endlb with
     | l :: _ -> jmp l ctx
     | [] -> raise (CodeGenError "not in a loop"))
  | Continue ->
    (match ctx.startlb with
     | l :: _ -> jmp l ctx
     | [] -> raise (CodeGenError "not in a loop"))

and gen_for init cond post body ctx =
  let lb0 = get_new_label ~name:"FORA" ctx in
    let lb1 = get_new_label ~name:"FORB" ctx in
      let lb2 = get_new_label ~name:"FORC" ctx in
        ctx
        |> inc_scope_level
        |> inc_labelc
        |> set_labels lb0 lb2
        |> init
        |> label lb0
        |> gen_exp cond
        |> cmpl "$0" "%eax"
        |> je lb2
        |> gen_statement body
        |> label lb1
        |> gen_exp post
        |> jmp lb0
        |> label lb2
        |> unset_labels
        |> deallocate_vars ctx

and gen_statements stas =
  match stas with
  | s :: ss ->
    gen_statement s >>
    gen_statements ss
  | [] -> Fn.id

let rec init_params i params ctx =
  match params with
  | (None, _) :: ps -> init_params (i + 1) ps ctx
  | (Some v, t) :: ps ->
    ctx
    |> push arg_regs.(i)
    |> add_var v t
    |> init_params (i + 1) ps
  | _ -> ctx

let gen_fun (f : fun_declaration) out =
  { fun_name = f.name ; index= -8;
    scope_levelc = 0; labelc = 0;
    startlb = [];  endlb = [];
    vars = []; out = out }
  |> globel_label f.name
  |> label f.name
  |> push "%rbp"
  |> movw "%rsp" "%rbp"
  |> init_params 0 f.params
  |> gen_statements f.body

let gen_temp_lib out =
  { fun_name = "println" ; index= -8;
    scope_levelc = 0; labelc = 0;
    startlb = [];  endlb = [];
    vars = []; out = out; }
  |> Templib.gen_lib

let rec gen_prog p out =
  match p with
  | Prog [] -> ();
  | Prog (f :: fs) ->
      let _ = gen_fun f out in
        Out_channel.newline out;
          gen_prog (Prog fs) out
