
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Drop-In replacement to Stdlib} *)

module Array = CCArray
module ArrayLabels = CCArrayLabels
module Array_slice = CCArray_slice
module Bool = CCBool
module Char = struct
  include Char
  include (CCChar : module type of CCChar with type t := t)
end
module Equal = CCEqual
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
  include CCHashtbl.Poly
  module type S' = CCHashtbl.S
  module Make' = CCHashtbl.Make
end
module Heap = CCHeap
module List = CCList
module ListLabels = CCListLabels
module Map = struct
  module type OrderedType = Map.OrderedType
  include CCMap
end
module Option = CCOpt
module Ord = CCOrd
module Pair = CCPair
module Parse = CCParse
module Random = CCRandom
module Ref = CCRef
module Result = struct
  include Result
  include CCResult
end
module Set = struct
  module type OrderedType = Set.OrderedType
  include CCSet
end
module String = CCString
module Vector = CCVector

include CCPervasives
