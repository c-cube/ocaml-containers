

module A = struct
  (* test consistency of interfaces *)
  module FA = CCShimsArray_.Floatarray
  module type L = module type of CCArray with module Floatarray := FA
  module type LL = module type of CCArrayLabels with module Floatarray := FA

  let () = ignore (module CCArrayLabels : L)

  let () = ignore (module CCArray : LL)
end

module S = struct
  (* test consistency of interfaces *)
  module type L = module type of CCString
  module type LL = module type of CCStringLabels

  let () = ignore (module CCStringLabels : L)

  let () = ignore (module CCString : LL)

end

module L = struct
  (* test consistency of interfaces *)
  module type L = module type of CCList
  module type LL = module type of CCListLabels
  let () = ignore (module CCListLabels : L)

  let () = ignore (module CCList : LL)
end

let () = print_endline "labelled modules are consistent ✔"
