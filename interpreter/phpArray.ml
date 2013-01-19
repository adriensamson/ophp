class phpArray =
    object (self : Language.Typing.value #Language.Typing.phpArray)
        val hashTable = Hashtbl.create 10
        val mutable indexList = []
        val mutable currentIndex = None
        val mutable nextNumericOffset = 0
        method offsetExists off = Hashtbl.mem hashTable off
        method offsetGet off = Hashtbl.find hashTable off
        method offsetSet off value =
            let offset = match off with
                | Some s ->
                    if Str.string_match (Str.regexp "[0-9]+") s 0 then
                        let i = int_of_string (Str.matched_string s) in
                        nextNumericOffset <- max nextNumericOffset (i+1)
                    else ();
                    s
                | None ->
                    let s = string_of_int nextNumericOffset in
                    nextNumericOffset <- nextNumericOffset + 1;
                    s
            in
            if not (List.mem offset indexList) then
                indexList <- indexList @ [offset]
            else ();
            Hashtbl.replace hashTable offset value
        method offsetUnset off =
            let rec list_remove l e = match l with
                | [] -> []
                | a::t when a = e -> t
                | a::t -> a::(list_remove t e)
            in
            indexList <- list_remove indexList off;
            Hashtbl.remove hashTable off
        
        method current () = match currentIndex with
            | None -> raise Not_found
            | Some k -> Hashtbl.find hashTable k
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
    end
