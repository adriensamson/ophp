class ['v] phpArray =
    object (self)
        inherit [string, 'v Language.Typing.variable] Bag.bag 10 as parent
        val mutable indexList = []
        val mutable currentIndex = None
        val mutable nextNumericOffset = 0

        method get off =
            try
                parent#get off
            with
            | Not_found -> failwith (Printf.sprintf "Offset %s not found" off)

        method set off v =
            self#registerOffset off;
            if not (List.mem off indexList) then
                indexList <- indexList @ [off]
            ;
            parent#set off v

        method setOption op v = match op with
            | Some off -> self#set off v
            | None -> self#set self#nextOffset v

        method remove off =
            let rec list_remove l e = match l with
                | [] -> []
                | a::t when a = e -> t
                | a::t -> a::(list_remove t e)
            in
            indexList <- list_remove indexList off;
            parent#remove off
        
        method private registerOffset s =
            if Str.string_match (Str.regexp "[0-9]+") s 0 then
            let i = int_of_string (Str.matched_string s) in
            nextNumericOffset <- max nextNumericOffset (i+1)
        method nextOffset =
            let s = string_of_int nextNumericOffset in
            nextNumericOffset <- nextNumericOffset + 1;
            s
        
        method current () = match currentIndex with
            | None -> failwith "Invalid index"
            | Some k -> parent#get k
        method key () = match currentIndex with
            | None -> failwith "Invalid index"
            | Some k -> k
        method next () = match currentIndex with
            | None -> ()
            | Some a -> let rec do_next l = match l with
                | [] -> assert false
                | [b] when b = a -> currentIndex <- None
                | [_] -> assert false
                | b::c::_ when b = a -> currentIndex <- Some c
                | b::t -> do_next t
            in do_next indexList
        method rewind () = match indexList with
            | [] -> currentIndex <- None
            | a::_ -> currentIndex <- Some a
        method valid () = match currentIndex with 
            | None -> false
            | Some _ -> true
        
        method count () = List.length indexList
        
        method copy () =
        let newHashTable = Hashtbl.create 10 in
        let f k v =
            let newVal = match v#get with
            | `Array arr -> `Array (arr#copy ())
            | nv -> nv
            in
            Hashtbl.replace newHashTable k (new Language.Typing.variable newVal)
        in
        Hashtbl.iter f hashTable;
        {< hashTable = newHashTable >}
        
        method keys = indexList
        method all = List.map (fun k -> k, self#get k) indexList
        
        method sort compare =
            indexList <- List.stable_sort compare indexList
    end

let sortCompare arr k1 k2 =
    match Expression.compare_values (arr#get k1)#get (arr#get k2)#get with
    | None -> 0
    | Some i -> i

