open Language.Typing
open Language.Ast

exception NotTraversable

type exec_return =
    | NoOp
    | Return of value
    | Break of int
    | Continue of int

let is_break op = match op with Break _ -> true | _ -> false

let echo v = match v with
    | `Null -> ()
    | `String s -> print_string s
    | `Bool b -> print_string (string_of_bool b)
    | `Double f -> print_float f
    | `Long i -> print_int i
    | `Array _ -> print_string "Array(...)"

let rec exec v s = match s with
    | IgnoreResult e -> let _ = eval v e in NoOp
    | Language.Ast.Return e -> Return (eval v e)
    | Language.Ast.Break i -> Break i
    | Language.Ast.Continue i -> Continue i
    | FunctionDef (name, argNames, code) ->
        let f argValues =
            let localVars = Hashtbl.create 10 in
            List.iter2 (fun name value -> Hashtbl.add localVars name value) argNames argValues;
            match exec_list localVars code with
                | Return v -> v
                | _ -> `Null
        in Function.registry#add name f; NoOp
    | Echo e -> echo (eval v e); NoOp
    | If (e, sl) -> let `Bool cond = to_bool (eval v e) in if cond then exec_list v sl else NoOp
    | IfElse (e, sl1, sl2) -> let `Bool cond = to_bool (eval v e) in if cond then exec_list v sl1 else exec_list v sl2
    | While (e, sl) -> begin
        let result = ref NoOp in
        while not (is_break !result) && let `Bool cond = to_bool (eval v e) in cond do
            result := exec_list v sl
        done;
        match !result with
            | Break i when i <= 1 -> NoOp
            | Continue i when i <= 1 -> NoOp
            | Break i -> Break (i-1)
            | Continue i -> Continue (i-1)
            | _ -> !result
    end
    | For (e_init, e_end, e_loop, sl) -> begin
        let result = ref NoOp in
        let rec eval_all es = match es with
            | [] -> `Bool true
            | [e] -> to_bool (eval v e)
            | e::l -> let _ = eval v e in eval_all l
        in
        let _ = eval_all e_init in
        while not (is_break !result) && let `Bool cond = eval_all e_end in cond do
            result := exec_list v sl;
            if not (is_break !result) then
                let _ = eval_all e_loop in ()
            else ()
        done;
        match !result with
            | Break i when i <= 1 -> NoOp
            | Continue i when i <= 1 -> NoOp
            | Break i -> Break (i-1)
            | Continue i -> Continue (i-1)
            | _ -> !result
    end
    | Foreach (e, ko, vn, sl) -> begin match eval v e with
        | `Array a -> begin
            a#rewind ();
            let result = ref NoOp in
            while not (is_break !result) && a#valid() do
                Hashtbl.replace v vn (a#current ());
                begin match ko with
                    | None -> ()
                    | Some kn -> match a#key () with
                        | None -> assert false
                        | Some k -> Hashtbl.replace v kn (`String k)
                end;
                result := exec_list v sl;
                if not (is_break !result) then
                    a#next ()
                else ()
            done;
            match !result with
                | Break i when i <= 1 -> NoOp
                | Continue i when i <= 1 -> NoOp
                | Break i -> Break (i-1)
                | Continue i -> Continue (i-1)
                | _ -> !result
        end
        | _ -> raise NotTraversable
    end

and exec_list v sl = match sl with
    | [] -> NoOp
    | a::t -> match exec v a with
        | NoOp -> exec_list v t
        | r -> r
and eval v e = Expression.eval v e
