open Language.Typing
open Language.Ast

exception NotTraversable

type exec_return =
    | NoOp
    | Return of value
    | Break of int
    | Continue of int

let is_break op = match op with Break _ -> true | _ -> false

let rec exec v s = match s with
    | IgnoreResult e -> let _ = eval v e in NoOp
    | Language.Ast.Return e -> Return (eval v e)
    | Language.Ast.Break i -> Break i
    | Language.Ast.Continue i -> Continue i
    | FunctionDef (name, argNames, code) ->
        let f argValues =
            let localVars = new Variable.variableRegistry in
            List.iter2 (fun name value -> localVars#set name value) argNames argValues;
            match exec_list (Expression.makeContext localVars) code with
                | Return v -> v
                | _ -> `Null
        in Registry.functions#add name f; NoOp
    | ClassDef (className, isStatic, isAbstract, isFinal, isInterface, parentName, implementsNames, contents) ->
        let constants = ref [] in
        let properties = ref [] in
        let methods = ref [] in
        let staticMethods = ref [] in
        let abstractMethods = ref [] in
        let f c = match c with
            | ConstantDef (name, e) -> constants := (name, eval v e)::!constants
            | PropertyDef (name, isStatic, visibility, init) -> let inited = match init with None -> `Null | Some i -> eval v i in properties := (name, isStatic, visibility, inited)::!properties
            | MethodDef (name, isStatic, visibility, argNames, code) ->
                if isStatic then begin
                    let f inClass finalClass argValues =
                        let localVars = new Variable.variableRegistry in
                        List.iter2 (fun name value -> localVars#set name value) argNames argValues;
                        match exec_list (Expression.makeContext ~callingClass:inClass localVars) code with
                            | Return v -> v
                            | _ -> `Null
                    in
                    staticMethods := (name, visibility, f)::!staticMethods
                end else begin
                    let f inClass obj argValues =
                        let localVars = new Variable.variableRegistry in
                        List.iter2 (fun name value -> localVars#set name value) argNames argValues;
                        match exec_list (Expression.makeContext ~obj ~callingClass:inClass localVars) code with
                            | Return v -> v
                            | _ -> `Null
                    in
                    methods := (name, visibility, f)::!methods
                end
            | AbstractMethodDef (name, isStatic, visibility, argNames) -> abstractMethods := (name, isStatic, visibility, argNames)::!abstractMethods
        in
        List.iter f contents;
        let parent = match parentName with None -> None | Some n -> Some (Registry.classes#get n) in
        let implements = List.map (Registry.classes#get) implementsNames in
        Registry.classes#add className (new Object.phpClass className isStatic isAbstract isFinal isInterface parent implements !constants !properties !methods !staticMethods !abstractMethods);
        NoOp
    | Echo e ->
        let `String s = to_string (eval v e) in
        print_string s;
        NoOp
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
                (v.Expression.vars)#set vn (a#current ());
                begin match ko with
                    | None -> ()
                    | Some kn -> let k = a#key () in (v.Expression.vars)#set kn (`String k)
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
