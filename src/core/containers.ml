(* This file is free software, part of containers. See file "license" for more details. *)

(** Drop-In replacement to Stdlib *)

module Array = CCArray
module Bool = CCBool
module Byte_buffer = CCByte_buffer
module Char = CCChar
module Equal = CCEqual
module Either = CCEither
module Float = CCFloat
module Format = CCFormat
module Fun = CCFun
module Hash = CCHash

(** @since 0.14 *)
module Hashtbl = struct
  include (
    Hashtbl :
      module type of Hashtbl
        with type statistics = Hashtbl.statistics
         and module Make = Hashtbl.Make
         and type ('a, 'b) t = ('a, 'b) Hashtbl.t)

  include CCHashtbl.Poly

  module type S' = CCHashtbl.S

  module Make' = CCHashtbl.Make
end

module Heap = CCHeap
module Int = CCInt
module Int32 = CCInt32
module Int64 = CCInt64
module IO = CCIO
module List = CCList
module Map = CCMap
module Nativeint = CCNativeint
module Option = CCOption
module Ord = CCOrd
module Pair = CCPair
module Parse = CCParse
module Random = CCRandom
module Ref = CCRef
module Result = CCResult
module Seq = CCSeq
module Set = CCSet
module String = CCString
module Vector = CCVector
module Monomorphic = CCMonomorphic
module Utf8_string = CCUtf8_string
module Unit = CCUnit
module Atomic = CCAtomic
module Sexp = CCSexp
module Sexp_intf = CCSexp_intf
module Canonical_sexp = CCCanonical_sexp
module Stdlib = CCShims_.Stdlib
include Monomorphic
