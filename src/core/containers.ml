
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Drop-In replacement to Stdlib} *)

module Array = CCArray
module ArrayLabels = CCArrayLabels
module Array_slice = CCArray_slice
module Bool = CCBool
module Char = Char
module Equal = CCEqual
module Float = CCFloat
module Format = CCFormat
module Fun = CCFun
module Hash = CCHash

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
module Int = CCInt
module Int64 = CCInt64
module IO = CCIO
module List = CCList
module ListLabels = CCListLabels
module Map = CCMap
module Option = CCOpt
module Ord = CCOrd
module Pair = CCPair
module Parse = CCParse
module Random = CCRandom
module Ref = CCRef
module Result = CCResult
module Set = CCSet
module String = CCString
module Vector = CCVector
module Monomorphic = CCMonomorphic

include Monomorphic
