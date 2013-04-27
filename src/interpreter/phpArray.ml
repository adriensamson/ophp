class ['v] variable (value : 'v) =
    object
    val mutable v = value
    method get = v
    method set nv = v <- nv
    end

class ['v] phpArray =
    object (self)
        val hashTable = Hashtbl.create 10
        val mutable indexList = []
        val mutable currentIndex = None
        val mutable nextNumericOffset = 0
        method offsetExists off = Hashtbl.mem hashTable off
        method offsetGet off = (Hashtbl.find hashTable off)#get
        method offsetSet off (value : 'v) =
            let offset = match off with
                | Some s ->
                    self#registerOffset s;
                    s
                | None ->
                    self#nextOffset
            in
            if not (List.mem offset indexList) then
                indexList <- indexList @ [offset]
            else ();
            let var = new variable value
            in
            Hashtbl.replace hashTable offset var
        method offsetVar off = Hashtbl.find hashTable off
        method offsetVarSet off v = self#registerOffset off; Hashtbl.replace hashTable off v
        method offsetUnset off =
            let rec list_remove l e = match l with
                | [] -> []
                | a::t when a = e -> t
                | a::t -> a::(list_remove t e)
            in
            indexList <- list_remove indexList off;
            Hashtbl.remove hashTable off
        
        method private registerOffset s =
            if Str.string_match (Str.regexp "[0-9]+") s 0 then
            let i = int_of_string (Str.matched_string s) in
            nextNumericOffset <- max nextNumericOffset (i+1)
        method nextOffset =
            let s = string_of_int nextNumericOffset in
            nextNumericOffset <- nextNumericOffset + 1;
            s
        
        method current () = match currentIndex with
            | None -> raise Not_found
            | Some k -> (Hashtbl.find hashTable k)#get
        method key () = match currentIndex with
            | None -> raise Not_found
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
            Hashtbl.replace newHashTable k (new variable newVal)
        in
        Hashtbl.iter f hashTable;
        {< hashTable = newHashTable >}
        
        method keys = indexList
    end
