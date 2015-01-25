type +'a t = [`Node of 'a * 'a t list]

type 'a tree = 'a t

let rec fold ~f init_acc (`Node (value, children)) =
  let acc = f value init_acc in
  List.fold_left (fun acc' child_node -> fold ~f acc' child_node) acc children

let split_at_length_minus_1 l =
  let rev_list = List.rev l in
  match rev_list with
  | []          -> (l, None)
  | [item]      -> ([], Some item)
  | item::items -> (List.rev items, Some item)

let print formatter string_of_value tree =
  let rec print_children children indent_string =
    let non_last_children, maybe_last_child =
      split_at_length_minus_1 children
    in
    print_non_last_children non_last_children indent_string;
    match maybe_last_child with
    | Some last_child -> print_last_child last_child indent_string;
    | None            -> ();
  and print_non_last_children non_last_children indent_string =
    List.iter (fun (`Node (child_value, grandchildren)) ->
        Format.pp_print_string formatter indent_string;
        Format.pp_print_string formatter "|- ";
        string_of_value child_value |> Format.pp_print_string formatter;
        Format.pp_force_newline formatter ();
        let indent_string' = indent_string ^ "|  " in
        print_children grandchildren indent_string'
      ) non_last_children;
  and print_last_child (`Node (last_child_value, last_grandchildren)) indent_string =
    Format.pp_print_string formatter indent_string;
    Format.pp_print_string formatter "'- ";
    string_of_value last_child_value |> Format.pp_print_string formatter;
    Format.pp_force_newline formatter ();
    let indent_string' = indent_string ^ "   " in
    print_children last_grandchildren indent_string'
  in
  let print_root (`Node (root_value, root_children)) =
    string_of_value root_value |> Format.pp_print_string formatter;
    Format.pp_force_newline formatter ();
    print_children root_children ""
  in
  print_root tree;
  Format.pp_print_flush formatter ()

module Zipper = struct

  type 'a parent = { left_siblings: ('a tree) list; value: 'a; right_siblings: ('a tree) list }

  type 'a t = { tree: 'a tree; lefts: ('a tree) list; rights: ('a tree) list; parents: ('a parent) list }

  let zipper tree = { tree = tree ; lefts = []; rights = []; parents = [] }

  let tree zipper = zipper.tree

  let left_sibling zipper =
    let rev_lefts = List.rev zipper.lefts in
    match rev_lefts with
    | []    -> None
    | last_left::tail_rev_lefts ->
      Some {
        tree = last_left ;
        lefts = tail_rev_lefts |> List.rev ;
        rights = zipper.tree::zipper.rights ;
        parents = zipper.parents
      }

  let right_sibling zipper =
    match zipper.rights with
    | []                  -> None
    | right::other_rights ->
      Some {
        tree = right ;
        lefts = zipper.tree::zipper.lefts ;
        rights = other_rights ;
        parents = zipper.parents ;
      }

  let parent zipper =
    match zipper.parents with
    | []                    -> None
    | { left_siblings ; value ; right_siblings }::other_parents ->
      Some {
        tree = `Node (value, zipper.lefts @ [zipper.tree] @ zipper.rights) ;
        lefts = left_siblings ;
        rights = right_siblings ;
        parents = other_parents ;
      }

  let rec root zipper =
    let maybe_parent_zipper = parent zipper in
    match maybe_parent_zipper with
    | None                -> zipper
    | Some parent_zipper  -> root parent_zipper

  let nth_child n ({ tree = `Node (value, children) ; _ } as zipper ) =
    let lefts, maybe_child, rev_rights, counter = List.fold_left (
        fun (lefts, maybe_child, rev_rights, counter) tree ->
          let lefts', maybe_child', rev_rights' =
            match counter with
            | _ when counter == n -> (lefts, Some tree, [])
            | _ when counter < n ->
              (tree::lefts, None, [])
            | _                   ->
              (lefts, maybe_child, tree::rev_rights)
          in
          (lefts', maybe_child', rev_rights', counter+1)
      ) ([], None, [], 0) children
    in
    begin match maybe_child with
      | Some child  ->
        Some {
          tree = child ;
          lefts = List.rev lefts;
          rights = List.rev rev_rights ;
          parents = {
            left_siblings = zipper.lefts ;
            value = value ;
            right_siblings = zipper.rights ;
          }::zipper.parents ;
        }
      | None        -> None
    end

  let append_child tree ({ tree = `Node (value, children) ; _ } as zipper ) =
    {
      tree ;
      lefts = children ;
      rights = [] ;
      parents = {
        left_siblings = zipper.lefts ;
        value = value ;
        right_siblings = zipper.rights ;
      }::zipper.parents ;
    }

  let insert_left_sibling tree zipper =
    match zipper.parents with
    | []  -> None
    | _   -> Some { zipper with tree ; rights = zipper.tree::zipper.rights }

  let insert_right_sibling tree zipper =
    match zipper.parents with
    | []  -> None
    | _   -> Some { zipper with tree ; lefts = zipper.tree::zipper.lefts }

  let replace tree zipper =
    { zipper with tree }

  let delete ({ tree = `Node (value, children) ; _ } as zipper ) =
    match zipper with
    | { lefts = first_left::other_lefts ; _  }     ->
      Some { zipper with tree = first_left ; lefts = other_lefts }
    | { rights = first_right::other_rights ; _ }  ->
      Some { zipper with tree = first_right ; rights = other_rights }
    | { parents = { left_siblings ; value ; right_siblings }::other_parents ; _ } ->
      Some {
        tree = `Node (value, zipper.lefts @ zipper.rights) ;
        lefts = left_siblings ;
        rights = right_siblings ;
        parents = other_parents ;
      }
    | _ -> None
end
