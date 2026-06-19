open! Core
open Bonsai_web

let component _graph =
  Bonsai.const
    (Vdom.Node.div
       ~attrs:[ Vdom.Attr.style (Css_gen.font_family [ "system-ui" ]) ]
       [ Vdom.Node.h1 [ Vdom.Node.text "Burpee Pose Labeller" ]
       ; Vdom.Node.p
           [ Vdom.Node.text
               "Load a Burpee Trainer pose export bundle, replay traces, and label reps/phases."
           ]
       ])
;;

let () = Bonsai_web.Start.start component
