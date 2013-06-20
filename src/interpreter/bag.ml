class ['a, 'b] bag n =
object (self)
    val hashTable = (Hashtbl.create n : ('a, 'b) Hashtbl.t)

    method get = Hashtbl.find hashTable
    method set = Hashtbl.replace hashTable
    method has = Hashtbl.mem hashTable
    method remove = Hashtbl.remove hashTable
    method keys = Hashtbl.fold (fun a _ l -> a::l) hashTable []
    method all = Hashtbl.fold (fun a b l -> (a,b)::l) hashTable []
end

