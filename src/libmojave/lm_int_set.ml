module OrderedInt =
 struct
   type t = int
   let compare (i : int) (j : int) =
     Pervasives.compare i j
end

module IntSet = Lm_set.LmMake (OrderedInt)
module IntTable = Lm_map.LmMake (OrderedInt)
module IntMTable = Lm_map.LmMakeList (OrderedInt)

