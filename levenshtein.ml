
(*
copyright (c) 2013, simon cruanes
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


(** {1 Levenshtein distance} *)

module NDA = struct
  type 'a char =
    | Any
    | Char of 'a
  type 'a transition =
    | Success
    | Upon of 'a char * int * int
    | Epsilon of int * int

  (* non deterministic automaton *)
  type 'a t = {
    compare : 'a -> 'a -> int;
    matrix : 'a transition list array array;
  }

  let length nda = Array.length nda.matrix

  let get_compare nda = nda.compare

  (* build NDA from the "get : int -> 'a" function *)
  let make ~compare ~limit ~len ~get =
    let m = Array.make_matrix len limit [] in
    let add_transition i j tr =
      m.(i).(j) <- tr :: m.(i).(j)
    in
    (* internal transitions *)
    for i = 0 to len-1 do
      for j = 0 to limit do
        (* correct char *)
        add_transition i j (Upon (Char (get i), i+1, j));
        (* other transitions *)
        if j < limit then begin
          (* substitution *)
          add_transition i j (Upon (Any, i+1, j+1)); 
          (* deletion in indexed string *)
          add_transition i j (Upon (Any, i, j+1));
          (* addition to indexed string *)
          add_transition i j (Epsilon (i+1, j+1));
        end
      done
    done;
    for j = 0 to limit do
      (* deletions at the end *)
      if j < limit
        then add_transition (len-1) j (Upon (Any, len-1, j+1));
      (* win in any case *)
      add_transition (len-1) j Success;
    done;
    { matrix=m; compare; }

  let get nda (i,j) =
    nda.matrix.(i).(j)
end

(** deterministic automaton *)
module DFA = struct
  type 'a transition =
    | Success
    | Upon of 'a * int  (* transition to state i *)
    | Otherwise of int  (* transition to state i *)

  type 'a t = {
    compare : 'a -> 'a -> int;
    mutable transitions : 'a transition list array;
    mutable len : int;
  }

  let create ~compare size = {
    compare;
    len = 0;
    transitions = Array.make size [];
  }

  (* add a new state *)
  let add_state dfa =
    let n = dfa.len in
    (* resize *)
    if n = Array.length dfa.transitions then begin
      let a' = Array.make (2*n) [] in
      Array.blit dfa.transitions 0 a' 0 n;
      dfa.transitions <- a'
    end;
    dfa.len <- n + 1;
    n

  (* add transition *)
  let add_transition dfa i tr =
    dfa.transitions.(i) <- tr :: dfa.transitions.(i)

  (* set of pairs of ints: used for representing a set of states of the NDA *)
  module NDAStateSet = Set.Make(struct
    type t = int * int
    let compare = Pervasives.compare
  end)

  (* list of characters that can specifically be followed from the given set *)
  let chars_from_set nda set =
    NDAStateSet.fold
      (fun state acc ->
        let transitions = NDA.get nda state in
        List.fold_left
          (fun acc tr -> match tr with
            | NDA.Upon (NDA.Char c, _, _) ->
                if List.exists (fun c' -> nda.NDA.compare c c' = 0) acc
                then acc
                else c :: acc (* new char! *)
            | _ -> acc
          ) acc transitions
      ) set []

  (* saturate current set w.r.t epsilon links *)
  let saturate_epsilon nda set =
    let q = Queue.create () in
    NDAStateSet.iter (fun s -> Queue.push s q) set;
    let set = ref set in
    while not (Queue.is_empty q) do
      let state = Queue.pop q in
      List.iter
        (fun tr' -> match tr' with
          | NDA.Epsilon (i,j) ->
              if not (NDAStateSet.mem (i,j) !set)
              then (set := NDAStateSet.add (i,j) !set; Queue.push (i,j) q)
          | _ -> ()
        ) (NDA.get nda state)
    done;
    !set

  exception LeadToSuccess

  (* find the transition that matches the given char (if any);
     may raise exceptions Not_found or LeadToSuccess. *)
  let rec get_transition_for_char nda c transitions =
    match c, transitions with
    | _, NDA.Success::_ -> raise LeadToSuccess
    | NDA.Char c', NDA.Upon (NDA.Char c'', i, j) :: transitions' ->
        if nda.NDA.compare c' c'' = 0
          then i, j
          else get_transition_for_char nda c transitions'
    | NDA.Any, NDA.Upon (NDA.Any, i, j) :: _ -> i, j
    | _, NDA.Upon (NDA.Any, i, j) :: transitions' ->
        begin try get_transition_for_char nda c transitions'
        with Not_found -> i, j (* only if no other transition works *)
        end
    | _, _::transitions' -> get_transition_for_char nda c transitions'
    | _, [] -> raise Not_found

  (* follow transition for given NDA.char, returns a new state
     and a boolean indicating whether it's final *)
  let follow_transition nda set c =
    let is_final = ref false in
    let set' = NDAStateSet.fold
      (fun state acc ->
        (* possible transitions *)
        let transitions = NDA.get nda state in
        try
          let state'  = get_transition_for_char nda c transitions in
          NDAStateSet.add state' acc
        with
        | LeadToSuccess -> is_final := true; acc
        | Not_found -> acc (* state dies *)
      ) set NDAStateSet.empty
    in
    let set' = saturate_epsilon nda set' in
    set', !is_final

  (* only follow "Any" transitions *)
  let follow_other_transition nda set =
    let is_final = ref false in
    let set' = NDAStateSet.fold
      (fun state acc ->
        (* possible transitions *)
        let transitions = NDA.get nda state in
        try
          let state' = get_transition_for_char nda NDA.Any transitions in
          NDAStateSet.add state' acc
        with
        | LeadToSuccess -> is_final := true; acc
        | Not_found -> acc (* state dies *)
      ) set NDAStateSet.empty
    in
    let set' = saturate_epsilon nda set' in
    set', !is_final

  (* call [k] with every [transition'] that can be reached from [set], with
     a bool that states whether it's final *)
  let iterate_transition_set nda set k =
    (* all possible "fixed char" transitions *)
    let chars = chars_from_set nda set in
    List.iter
      (fun c ->
        let set', is_final = follow_transition nda set (NDA.Char c) in
        k ~is_final (NDA.Char c) set')
      chars;
    (* remaining transitions, with only "Any" *)
    let set', is_final = follow_other_transition nda set in
    k ~is_final NDA.Any set'

  module StateSetMap = Map.Make(NDAStateSet)

  (* get the state that corresponds to the given set of NDA states.
    [states] is a map [nda set] -> [nfa state] *)
  let get_state dfa states set =
    try StateSetMap.find set !states
    with Not_found ->
      let i = add_state dfa in
      states := StateSetMap.add set i !states;
      i

  (* traverse the NDA. Currently we're at [set] *)
  let rec traverse nda dfa states set =
    let set = saturate_epsilon nda set in
    let set_i = get_state dfa states set in
    iterate_transition_set nda set
      (fun ~is_final c set' ->
        let set'_i = get_state dfa states set' in
        (* did we reach success? *)
        if is_final
          then add_transition dfa set'_i Success;
        (* link set -> set' *)
        match c with
        | NDA.Any ->
            add_transition dfa set_i (Otherwise set'_i)
        | NDA.Char c' ->
            add_transition dfa set_i (Upon (c', set'_i))
      )

  let of_nda nda =
    let compare = NDA.get_compare nda in
    let dfa = create ~compare (NDA.length nda) in
    (* map (set of NDA states) to int (state in DFA) *)
    let states = ref StateSetMap.empty in
    (* traverse the NDA to build the NFA *)
    traverse nda dfa states (NDAStateSet.singleton (0,0));
    dfa

  let get dfa i =
    dfa.transitions.(i)
end

type 'a automaton = 'a DFA.t

let of_array ?(compare=Pervasives.compare) ~limit a =
  let nda = NDA.make ~compare ~limit ~len:(Array.length a) ~get:(Array.get a) in
  let dfa = DFA.of_nda nda in
  dfa

let of_list ?compare ~limit l =
  of_array ?compare ~limit (Array.of_list l)

let of_string ~limit a =
  let compare = Char.compare in
  let nda = NDA.make ~compare ~limit ~len:(String.length a) ~get:(String.get a) in
  let dfa = DFA.of_nda nda in
  dfa

type match_result =
  | TooFar
  | Distance of int

exception FoundDistance of int

let rec __has_success = function
  | [] -> false
  | DFA.Success :: _ -> true
  | _ :: l' -> __has_success l'

let rec __find_char ~compare c l k = match l with
  | [] -> ()
  | DFA.Upon (c', next) :: l' ->
      if compare c c' = 0
      then k next
      else __find_char ~compare c l' k
  | _ :: l' -> __find_char ~compare c l' k

let rec __find_otherwise l k = match l with
  | [] -> ()
  | DFA.Otherwise next :: _ -> k next
  | _::l' -> __find_otherwise l' k

(* real matching function *)
let __match ~len ~get dfa =
  let rec search ~dist i state =
    if i = len then raise (FoundDistance dist)
    else begin
      let transitions = DFA.get dfa state in
      if __has_success transitions
        then raise (FoundDistance dist);
      (* current char *)
      let c = get i in
      __find_char ~compare:dfa.DFA.compare c transitions
        (fun next -> search ~dist (i+1) next);
      __find_otherwise transitions
        (fun next -> search ~dist:(dist+1) (i+1) next);
    end
  in
  try
    search ~dist:0 0 0;
    TooFar
  with FoundDistance i ->
    Distance i

let match_with dfa a =
  __match ~len:(Array.length a) ~get:(Array.get a) dfa

let match_with_string dfa s =
  __match ~len:(String.length s) ~get:(String.get s) dfa
