
(prover
  (name msat)
  (synopsis "msat for pure sat problems")
  (version "git:.")
  (sat "^Sat")
  (unsat "^Unsat")
  (cmd "$cur_dir/../msat.exe -time $timeout $file"))

(dir
  (path $cur_dir)
  (pattern ".*\\.cnf")
  (expect (const unknown)))
