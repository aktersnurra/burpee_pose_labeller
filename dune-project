(lang dune 3.14)
(name burpee_pose_labeller)
(generate_opam_files true)
(source
 (github aktersnurra/burpee_pose_labeller))
(package
 (name burpee_pose_labeller)
 (synopsis "OCaml/Bonsai pose trace labelling tool")
 (depends
  (ocaml (>= 5.1.0))
  (dune (>= 3.14))
  bonsai
  core
  ppx_jane
  js_of_ocaml
  js_of_ocaml-ppx))
