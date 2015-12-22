
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Simple Cache for Allocations} *)

module Arr = struct
  type 'a t = {
    caches: 'a array array;
      (* 2-dim array of cached arrays. The 2-dim array is flattened into
         one dimension *)
    max_buck_size: int;
      (* number of cached arrays per length *)
    sizes: int array;
      (* number of cached arrays in each bucket *)
  }

  let create ?(buck_size=16) n =
    if n<1 then invalid_arg "AllocCache.Arr.create";
    { max_buck_size=buck_size;
      sizes=Array.make n 0;
      caches=Array.make (n * buck_size) [||];
    }

  let make c i x =
    if i=0 then [||]
    else if i<Array.length c.sizes then (
      let bs = c.sizes.(i) in
      if bs = 0 then Array.make i x
      else (
        (* remove last array *)
        let ret = c.caches.(i * c.max_buck_size + bs-1) in
        c.sizes.(i) <- bs - 1;
        ret
      )
    ) else Array.make i x

  let free c a =
    let n = Array.length a in
    if n > 0 && n < Array.length c.sizes then (
      let bs = c.sizes.(n) in
      if bs < c.max_buck_size then (
        (* store [a] *)
        c.caches.(n * c.max_buck_size + bs) <- a;
        c.sizes.(n) <- bs + 1
      )
    )

  let with_ c i x ~f =
    let a = make c i x in
    try
      let ret = f a in
      free c a;
      ret
    with e ->
      free c a;
      raise e
end

(*$inject
  let c = Arr.create ~buck_size:2 20

*)

(*$Q
  Q.small_int (fun n -> Array.length (Arr.make c n '_') = n)
*)

(*$T
  let a = Arr.make c 1 '_' in Array.length a = 1
  let a = Arr.make c 2 '_' in Array.length a = 2
  let a = Arr.make c 3 '_' in Array.length a = 3
  let a = Arr.make c 4 '_' in Array.length a = 4
*)


