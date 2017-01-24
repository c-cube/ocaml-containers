
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Drop-In replacement to Stdlib}

This module is meant to be opened if one doesn't want to use both, say,
[List] and [CCList]. Instead, [List] is now an alias to
{[struct
    include List
    include CCList
  end
]}
*)

module Array = struct
  include Array
  include CCArray
end
module ArrayLabels = struct
  include ArrayLabels
  include CCArrayLabels
end
module Array_slice = CCArray_slice
module Bool = CCBool
module Char = struct
  include Char
  include (CCChar : module type of CCChar with type t := t)
end
module Float = CCFloat
module Format = struct
  include Format
  include CCFormat
end
module Fun = CCFun
module Hash = CCHash
module Int = CCInt
module Int64 = CCInt64
module IO = CCIO

(** @since 0.14 *)
module Hashtbl = struct
  include (Hashtbl : module type of Hashtbl
    with type statistics = Hashtbl.statistics
    and module Make = Hashtbl.Make
    and type ('a,'b) t = ('a,'b) Hashtbl.t
  )
  (* still unable to include CCHashtbl itself, for the polymorphic functions *)
  module type S' = CCHashtbl.S
  module Make' = CCHashtbl.Make
end
module Heap = CCHeap
module List = struct
  include List
  include CCList
end
module ListLabels = struct
  include ListLabels
  include CCListLabels
end
module Map = struct
  module type OrderedType = Map.OrderedType
  include CCMap
end
module Option = CCOpt
module Ord = CCOrd
module Pair = CCPair
module Parse = CCParse
module Random = struct
  include Random
  include CCRandom
end
module Ref = CCRef
module Result = struct
  include Result
  include CCResult
end
module Set = struct
  module type OrderedType = Set.OrderedType
  include CCSet
end
module String = struct
  include String
  include CCString
end
module Vector = CCVector

