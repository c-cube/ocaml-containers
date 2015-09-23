
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Drop-In replacement to Stdlib}

This module is meant to be opened if one doesn't want to use both, say,
[List] and [CCList]. Instead, [List] is now an alias to
{[struct
    include List
    include CCList
  end
]}

@since 0.4

Changed [Opt] to [Option] to better reflect that this module is about the
['a option] type, with [module Option = CCOpt].

@since 0.5

Renamed from [CCPervasives] in [containers.pervasives], to [Containers]
in the core library [containers]

@since 0.10
*)

module Array = struct
  include Array
  include CCArray
end
module Bool = CCBool
module Error = CCError
module Float = CCFloat
module Format = struct
  include Format
  include CCFormat
end
module Fun = CCFun
module Hash = CCHash
module Int = CCInt
(* FIXME
module Hashtbl = struct
  include (Hashtbl : module type of Hashtbl
    with type statistics = Hashtbl.statistics
    and module Make := Hashtbl.Make
    and module type S := Hashtbl.S
    and type ('a,'b) t := ('a,'b) Hashtbl.t
  )
  include CCHashtbl
end
*)
module List = struct
  include List
  include CCList
end
module Map = CCMap
module Option = CCOpt
module Pair = CCPair
module Random = struct
  include Random
  include CCRandom
end
module Ref = CCRef
module Set = CCSet
module String = struct
  include String
  include CCString
end
module Vector = CCVector

module Int64 = CCInt64
(** @since 0.13 *)
