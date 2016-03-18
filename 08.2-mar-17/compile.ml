open Printf
open Expr
open Instruction

type 'a envt = (string * 'a) list

let count = ref 0
let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

type hole =
  | CHole of (cexpr -> aexpr)
  | ImmHole of (immexpr -> aexpr)

let fill_imm (h : hole) (v : immexpr) : aexpr =
  match h with
    | CHole(k) -> (k (CImmExpr(v)))
    | ImmHole(k) -> (k v)

let fill_c (h : hole) (c : cexpr) : aexpr =
  match h with
    | CHole(k) -> (k c)
    | ImmHole(k) ->
      let tmp = gen_temp "" in
      ALet(tmp, c, k (ImmId(tmp)))

let return_hole = CHole(fun ce -> ACExpr(ce))

let rec anf_list (es : expr list) (k : immexpr list -> aexpr) : aexpr =
  match es with
    | [] -> k []
    | e::rest ->
      anf e (ImmHole(fun imm ->
        anf_list rest (fun imms -> k (imm::imms))))

and anf (e : expr) (h : hole) : aexpr =
  match e with
    | ENumber(n) -> fill_imm h (ImmNumber(n)) 
    | EBool(b) -> fill_imm h (ImmBool(b)) 
    | EId(x) -> fill_imm h (ImmId(x))
    | EPrim1(op, e) ->
      anf e (ImmHole(fun imm -> (fill_c h (CPrim1(op, imm)))))
    | EPrim2(op, left, right) ->
      anf left (ImmHole(fun limm ->
        anf right (ImmHole(fun rimm ->
          (fill_c h (CPrim2(op, limm, rimm)))))))
    | EPair(left, right) ->
      anf left (ImmHole(fun limm ->
        anf right (ImmHole(fun rimm ->
          (fill_c h (CPair(limm, rimm)))))))
    | EApp(f, args) ->
      anf_list args (fun aimms -> fill_c h (CApp(f, aimms)))
    | EIf(cond, thn, els) ->
      anf cond (ImmHole(fun cimm ->
        (fill_c h (CIf(cimm, (anf thn return_hole), (anf els return_hole))))))
    | ELet([], body) -> anf body h
    | ELet((name, value)::rest, body) ->
      anf value (CHole(fun ce ->
        ALet(name, ce, anf (ELet(rest, body)) h)))

let anf_decl (d : decl) : adecl =
  match d with
    | DFun(name, args, body) ->
      ADFun(name, args, anf body return_hole)

let anf_program (p : program) : aprogram =
  match p with
    | Program(decls, main) ->
      AProgram(List.map anf_decl decls, anf main return_hole)

let rec find ls x =
  match ls with
    | [] -> None
    | (y,v)::rest ->
      if y = x then Some(v) else find rest x

let const_true = HexConst(0xffffffff)
let const_false = HexConst(0x7fffffff)

let acompile_imm_arg (i : immexpr) _ (env : int envt) : arg =
  match i with
    | ImmNumber(n) ->
      Const((n lsl 1))
    | ImmBool(b) ->
      if b then const_true else const_false
    | ImmId(name) ->
      begin match find env name with
        | Some(stackloc) -> RegOffset(-4 * stackloc, EBP)
        | None -> failwith ("Unbound identifier" ^ name)
      end

let acompile_imm (i : immexpr) (si : int) (env : int envt) : instruction list =
  [ IMov(Reg(EAX), acompile_imm_arg i si env) ]

let throw_err code = 
  [
    IPush(Sized(DWORD_PTR, Const(code)));
    ICall("error");
  ]

let check_overflow = IJo("overflow_check")
let error_non_int = "error_non_int"
let error_non_bool = "error_non_bool"
let error_non_tuple = "error_non_tuple"
let error_too_small = "error_too_small"
let error_too_large = "error_too_large"


let check_num =
  [
    IAnd(Reg(EAX), Const(0x00000001));
    ICmp(Reg(EAX), Const(0x00000000));
    IJne(error_non_int)
  ]

let max n m = if n > m then n else m
let rec count_c_vars (ce : cexpr) : int =
  match ce with
    | CIf(_, thn, els) ->
      max (count_vars thn) (count_vars els)
    | _ -> 0

and count_vars (ae : aexpr) : int =
  match ae with
    | ALet(x, bind, body) -> 
      1 + (max (count_c_vars bind) (count_vars body))
    | ACExpr(ce) -> count_c_vars ce

let check_nums arg1 arg2 =
  [
    IMov(Reg(EAX), arg1) 
  ] @ check_num @ [
    IMov(Reg(EAX), arg2);
  ] @ check_num

let rec acompile_step (s : cexpr) (si : int) (env : int envt) : instruction list =
  match s with
    | CPair(left, right) ->
      let la = acompile_imm left si env in
      let ra = acompile_imm right si env in
      la @
      [
        IMov(RegOffset(0, ESI), Reg(EAX));
      ] @
      ra @
      [
        IMov(RegOffset(4, ESI), Reg(EAX));
        IMov(Reg(EAX), Reg(ESI));
        IAdd(Reg(ESI), Const(8));
        IAdd(Reg(EAX), Const(1));
      ]
    | CApp(f, iargs) ->
      let argpushes = List.rev_map (fun a -> IPush(Sized(DWORD_PTR, acompile_imm_arg a si env))) iargs in
      let esp_dist = 4 * (List.length iargs) in
      argpushes @ [
        ICall(f)
      ] @ [
        IAdd(Reg(ESP), Const(esp_dist))
      ]
    | CPrim1(op, e) ->
      let prelude = acompile_imm e si env in
      begin match op with
        | Fst ->
          prelude @
          (* check that EAX contains a pair *)
          (* Access the first value in the address at EAX - 1 *)
          [
            IMov(Reg(EAX), RegOffset(-1, EAX))
          ]
        | Snd ->
          [
            IMov(Reg(EAX), RegOffset(3, EAX))
          ]

        | Add1 ->
          prelude @ [
            IAdd(Reg(EAX), Const(2))
          ]
        | Sub1 ->
          prelude @ [
            IAdd(Reg(EAX), Const(-2))
          ]
        | IsNum ->
          prelude @ [
            IAnd(Reg(EAX), Const(0x00000001));
            IShl(Reg(EAX), Const(31));
            IXor(Reg(EAX), Const(0xFFFFFFFF));
          ]
        | IsBool ->
          prelude @ [
            IAnd(Reg(EAX), Const(0x00000001));
            IShl(Reg(EAX), Const(31));
            IOr(Reg(EAX), Const(0x7FFFFFFF));
          ]
        | Print ->
          prelude @ [
            IPush(Sized(DWORD_PTR, Reg(EAX)));
            ICall("print");
            IPop(Reg(EAX));
          ]
      end

    | CPrim2(op, left, right) ->
      let left_as_arg = acompile_imm_arg left si env in
      let right_as_arg = acompile_imm_arg right si env in
      let checked = check_nums left_as_arg right_as_arg in
      begin match op with
        | Plus ->
          checked @
          [
            IMov(Reg(EAX), left_as_arg);
            IAdd(Reg(EAX), right_as_arg);
            check_overflow
          ]
        | Minus ->
          checked @
          [
            IMov(Reg(EAX), left_as_arg);
            ISub(Reg(EAX), right_as_arg);
            check_overflow
          ]
        | Times ->
          checked @
          [
            IMov(Reg(EAX), left_as_arg);
            IShr(Reg(EAX), Const(1));
            IMul(Reg(EAX), right_as_arg);
            check_overflow;
          ]
        | Less ->
          checked @
          [
            IMov(Reg(EAX), left_as_arg);
            ISub(Reg(EAX), right_as_arg);
            ISub(Reg(EAX), Const(1));
            IAnd(Reg(EAX), HexConst(0x80000000));
            IOr( Reg(EAX), HexConst(0x7FFFFFFF));
          ]
        | Greater ->
          checked @
          [
            IMov(Reg(EAX), left_as_arg);
            ISub(Reg(EAX), right_as_arg);
            IAnd(Reg(EAX), HexConst(0x80000000));
            IXor(Reg(EAX), HexConst(0xFFFFFFFF));
          ]
        | Equal ->
          [
            IPush(Sized(DWORD_PTR, left_as_arg));
            IPush(Sized(DWORD_PTR, right_as_arg));
            ICall("equal");
            IAdd(Reg(ESP), Const(8));
          ]
        (*
          let leave_false = gen_temp "equals" in
          [
            IMov(Reg(EAX), left_as_arg);
            ICmp(Reg(EAX), right_as_arg);
            IMov(Reg(EAX), const_false);
            IJne(leave_false);
            IMov(Reg(EAX), const_true);
            ILabel(leave_false);
          ]
         *)
       end
    | CImmExpr(i) -> acompile_imm i si env
    | CIf(cond, thn, els) ->
      let prelude = acompile_imm cond si env in
      let thn = acompile_expr thn si env in
      let els = acompile_expr els si env in
      let label_then = gen_temp "then" in
      let label_else = gen_temp "else" in
      let label_end = gen_temp "end" in
      prelude @ [
        ICmp(Reg(EAX), const_true);
        IJe(label_then);
        ICmp(Reg(EAX), const_false);
        IJe(label_else);
        IJmp(error_non_bool);
        ILabel(label_then)
      ] @
      thn @
      [ IJmp(label_end); ILabel(label_else) ] @
      els @
      [ ILabel(label_end) ]

and acompile_expr (e : aexpr) (si : int) (env : int envt) : instruction list =
  match e with
    | ALet(id, e, body) ->
      let prelude = acompile_step e (si + 1) env in
      let postlude = acompile_expr body (si + 1) ((id, si)::env) in
      prelude @ [
        IMov(RegOffset(-4 * si, EBP), Reg(EAX))
      ] @ postlude
    | ACExpr(s) -> acompile_step s si env

let acompile_decl (ad : adecl) : instruction list =
  match ad with
    | ADFun(name, args, body) ->
      let varcount = count_vars body in
      let arglocs = List.mapi (fun i a -> (a, -1 * (i + 2))) args in
      [
        ILabel(name);
        IPush(Reg(EBP));
        IMov(Reg(EBP), Reg(ESP));
        ISub(Reg(ESP), Const(varcount * 4));
      ] @
      (acompile_expr body 1 arglocs) @
      [
        IMov(Reg(ESP), Reg(EBP));
        IPop(Reg(EBP));
        IRet;
      ]

let rec find_decl (ds : decl list) (name : string) : decl option =
  match ds with
    | [] -> None
    | (DFun(fname, _, _) as d)::ds_rest ->
      if name = fname then Some(d) else find_decl ds_rest name

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
    | [] -> false
    | x::xs -> (elt = x) || (find_one xs elt)

let rec find_dup (l : 'a list) : 'a option =
  match l with
    | [] -> None
    | [x] -> None
    | x::xs ->
      if find_one xs x then Some(x) else find_dup xs

let rec well_formed_e (e : expr) (ds : decl list) (env : bool envt) =
  match e with
    | ENumber(_)
    | EBool(_) -> []
    | EId(x) ->
      begin match find env x with
        | None -> ["Unbound identifier: " ^ x]
        | Some(_) -> []
      end
    | EPrim1(op, e) ->
      well_formed_e e ds env
    | EPrim2(op, left, right) ->
      (well_formed_e left ds env) @ (well_formed_e right ds env)
    | EPair(left, right) ->
      (well_formed_e left ds env) @ (well_formed_e right ds env)
    | EIf(cond, thn, els) ->
      (well_formed_e cond ds env) @
      (well_formed_e thn ds env) @
      (well_formed_e els ds env)
    | EApp(name, args) ->
      let from_args = List.flatten (List.map (fun a -> well_formed_e a ds env) args) in
      begin match find_decl ds name with
        | None -> ("No such function: " ^ name)::from_args
        | Some(_) -> from_args
      end
    | ELet(binds, body) ->
      let names = List.map fst binds in
      let env_from_binds = List.map (fun a -> (a, true)) names in
      let from_body = well_formed_e body ds (env_from_binds @ env) in
      begin match find_dup names with
        | None -> from_body
        | Some(name) -> ("Duplicate name in let: " ^ name)::from_body
      end

let well_formed_d (d : decl) (ds : decl list) : string list =
  match d with
    | DFun(name, args, body) ->
      let env = List.map (fun a -> (a, true)) args in
      let from_body = well_formed_e body ds env in
      begin match find_dup args with
        | None -> from_body
        | Some(v) -> ("Duplicate parameter: " ^ v)::from_body
      end

let well_formed_p (p : program) : string list =
  match p with
    | Program(ds, maine) ->
      let names = List.map (fun (DFun(name, _, _)) -> name) ds in
      let subexpr_errs = (well_formed_e maine ds []) @
        (List.flatten (List.map (fun d -> well_formed_d d ds) ds)) in
      begin match find_dup names with
        | None -> subexpr_errs
        | Some(v) -> ("Duplicate function definition: " ^ v)::subexpr_errs
      end

let compile_to_string prog =
  match well_formed_p prog with
    | x::rest ->
      let errstr = (List.fold_left (fun x y -> x ^ "\n" ^ y) "" (x::rest)) in
      failwith errstr
    | [] ->
      let anfed = (anf_program prog) in
      match anfed with
        | AProgram(decls, main) ->
          let compiled_decls = List.map acompile_decl decls in
          let compiled_main = (acompile_expr main 1 []) in
          let varcount = count_vars main in
          let stackjump = 4 * varcount in
          let prelude = "
section .text
extern error
extern print
extern equal
global our_code_starts_here" in
          let main_start = [
            ILabel("our_code_starts_here");

            IMov(Reg(ESI), RegOffset(4, ESP));

            IPush(Reg(EBP));
            IMov(Reg(EBP), Reg(ESP));
            ISub(Reg(ESP), Const(stackjump))
          ] in
          let postlude = [
            IMov(Reg(ESP), Reg(EBP));
            IPop(Reg(EBP));
            IRet;
            ILabel("overflow_check")
          ]
          @ (throw_err 3)
          @ [ILabel(error_non_int)] @ (throw_err 1)
          @ [ILabel(error_non_bool)] @ (throw_err 2)
          @ [ILabel(error_non_tuple)] @ (throw_err 4) in
          let as_assembly_string = (to_asm (
            (List.flatten compiled_decls) @
            main_start @
            compiled_main @
            postlude)) in
          sprintf "%s%s\n" prelude as_assembly_string

