type ImmutableStack<'T> =
    | Empty 
    | Stack of 'T * ImmutableStack<'T>

    member s.Push x = Stack(x, s)

    member s.Pop() = 
      match s with
      | Empty -> failwith "Underflow"
      | Stack(t,_) -> t

    member s.Top() = 
      match s with
      | Empty -> failwith "Contain no elements"
      | Stack(_,st) -> st

    member s.IEmpty = 
      match s with
      | Empty -> true
      | _ -> false

    member s.All() = 
      let rec loop acc = function
      | Empty -> acc
      | Stack(t,st) -> loop (t::acc) st
      loop [] s
