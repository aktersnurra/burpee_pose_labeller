open! Core
open Bonsai_web

module Labeller = Burpee_pose_labeller

let style value = Vdom.Attr.create "style" value

let sample_manifest_json =
  {|
{
  "version": 1,
  "captures": [
    {
      "capture_run_id": 42,
      "recorded_at": "2026-06-18T12:34:56Z",
      "session_name": "Lunch burpees",
      "has_warmup": true,
      "has_main": true,
      "warmup_sample_count": 120,
      "main_sample_count": 1800,
      "model_name": "hsmm-phase",
      "model_version": "2026-06-18",
      "labels_present": true,
      "analysis_present": true
    }
  ]
}
|}
;;

let capture_index_model =
  match Labeller.Bundle_manifest.parse_string sample_manifest_json with
  | Ok manifest -> Labeller.Model.apply_exn Labeller.Model.empty (Load_manifest manifest)
  | Error _ -> Labeller.Model.empty
;;

let text_or_dash = Option.value ~default:"—"

let segment_text capture =
  let has_warmup = Labeller.Capture_metadata.has_segment capture Warmup in
  let has_main = Labeller.Capture_metadata.has_segment capture Main in
  match has_warmup, has_main with
  | true, true -> "warmup + main"
  | true, false -> "warmup"
  | false, true -> "main"
  | false, false -> "no traces"
;;

let sample_text capture =
  let count segment =
    Labeller.Capture_metadata.sample_count capture segment
    |> Option.value_map ~default:"—" ~f:Int.to_string
  in
  [%string "main %{count Main} · warmup %{count Warmup}"]
;;

let pill text =
  Vdom.Node.span
    ~attrs:
      [ style
          "display:inline-flex;align-items:center;height:22px;padding:0 8px;border:1px solid #dedbd2;border-radius:999px;background:#fbfaf7;color:#6f6a60;font-size:12px;line-height:22px;"
      ]
    [ Vdom.Node.text text ]
;;

let status_pill ~active text =
  let colors =
    if active
    then "background:#f2efe7;color:#5f513f;border-color:#ddd5c6;"
    else "background:#fafafa;color:#9a958c;border-color:#e8e5df;"
  in
  Vdom.Node.span
    ~attrs:
      [ style
          [%string
            "display:inline-flex;align-items:center;height:22px;padding:0 8px;border:1px solid;border-radius:999px;font-size:12px;line-height:22px;%{colors}"]
      ]
    [ Vdom.Node.text text ]
;;

let capture_row capture =
  Vdom.Node.div
    ~attrs:
      [ style
          "display:grid;grid-template-columns:1fr auto;gap:16px;align-items:center;padding:14px 0;border-bottom:1px solid #ece9e2;"
      ]
    [ Vdom.Node.div
        [ Vdom.Node.div
            ~attrs:[ style "display:flex;align-items:center;gap:8px;margin-bottom:6px;" ]
            [ Vdom.Node.div
                ~attrs:[ style "font-size:15px;font-weight:520;color:#2f2d29;" ]
                [ Vdom.Node.text (text_or_dash (Labeller.Capture_metadata.session_name capture)) ]
            ; pill (segment_text capture)
            ]
        ; Vdom.Node.div
            ~attrs:[ style "font-size:12px;color:#8a857c;" ]
            [ Vdom.Node.text
                [%string
                  "%{text_or_dash (Labeller.Capture_metadata.recorded_at capture)} · %{sample_text capture}"]
            ]
        ]
    ; Vdom.Node.div
        ~attrs:[ style "display:flex;gap:6px;justify-content:flex-end;" ]
        [ status_pill ~active:(Labeller.Capture_metadata.labels_present capture) "labels"
        ; status_pill ~active:(Labeller.Capture_metadata.analysis_present capture) "analysis"
        ]
    ]
;;

let capture_index captures =
  Vdom.Node.section
    ~attrs:
      [ style
          "background:#fffefc;border:1px solid #e7e3da;border-radius:10px;padding:22px 24px;box-shadow:0 1px 1px rgba(15,15,15,0.03);"
      ]
    [ Vdom.Node.div
        ~attrs:[ style "display:flex;align-items:flex-end;justify-content:space-between;margin-bottom:14px;" ]
        [ Vdom.Node.div
            [ Vdom.Node.div
                ~attrs:[ style "font-size:12px;color:#8d877d;margin-bottom:6px;" ]
                [ Vdom.Node.text "Capture index" ]
            ; Vdom.Node.h2
                ~attrs:[ style "font-size:20px;line-height:1.2;margin:0;color:#25231f;font-weight:560;" ]
                [ Vdom.Node.text "Pose export bundle" ]
            ]
        ; Vdom.Node.div
            ~attrs:[ style "font-size:12px;color:#8d877d;" ]
            [ Vdom.Node.text [%string "%{List.length captures#Int} capture"] ]
        ]
    ; (match captures with
       | [] ->
         Vdom.Node.div
           ~attrs:
             [ style
                 "padding:42px 12px;color:#8d877d;font-size:14px;text-align:center;border-top:1px solid #ece9e2;"
             ]
           [ Vdom.Node.text "Open a bundle to see captured pose traces here." ]
       | captures -> Vdom.Node.div (List.map captures ~f:capture_row))
    ]
;;

let workspace_preview =
  Vdom.Node.section
    ~attrs:
      [ style
          "min-height:320px;background:#fbfaf7;border:1px solid #e7e3da;border-radius:10px;padding:22px 24px;"
      ]
    [ Vdom.Node.div
        ~attrs:[ style "font-size:12px;color:#8d877d;margin-bottom:6px;" ]
        [ Vdom.Node.text "Workspace" ]
    ; Vdom.Node.h2
        ~attrs:[ style "font-size:20px;line-height:1.2;margin:0 0 14px;color:#25231f;font-weight:560;" ]
        [ Vdom.Node.text "Select a capture to label" ]
    ; Vdom.Node.p
        ~attrs:[ style "max-width:420px;margin:0;color:#6f6a60;font-size:14px;line-height:1.65;" ]
        [ Vdom.Node.text
            "The replay canvas, timeline, and interval editor will live here. For now this shell proves the capture index can render bundle metadata in the app."
        ]
    ]
;;

let component _graph =
  let captures = Labeller.Model.captures capture_index_model in
  Bonsai.const
    (Vdom.Node.div
       ~attrs:
         [ style
             "min-height:100vh;background:#f7f5f0;color:#25231f;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;"
         ]
       [ Vdom.Node.main
           ~attrs:[ style "max-width:1120px;margin:0 auto;padding:56px 28px 72px;" ]
           [ Vdom.Node.header
               ~attrs:[ style "margin-bottom:28px;" ]
               [ Vdom.Node.div
                   ~attrs:[ style "font-size:13px;color:#8d877d;margin-bottom:8px;" ]
                   [ Vdom.Node.text "Burpee Pose Labeller" ]
               ; Vdom.Node.h1
                   ~attrs:
                     [ style
                         "font-size:34px;line-height:1.12;margin:0 0 10px;color:#1f1d1a;font-weight:620;letter-spacing:-0.02em;"
                     ]
                   [ Vdom.Node.text "A quiet lab notebook for pose traces." ]
               ; Vdom.Node.p
                   ~attrs:[ style "max-width:620px;margin:0;color:#6f6a60;font-size:15px;line-height:1.65;" ]
                   [ Vdom.Node.text
                       "Replay exported captures, inspect model overlays, and keep manual labels clean enough for training workflows."
                   ]
               ]
           ; Vdom.Node.div
               ~attrs:[ style "display:grid;grid-template-columns:minmax(0,1.1fr) minmax(320px,0.9fr);gap:18px;align-items:start;" ]
               [ capture_index captures; workspace_preview ]
           ]
       ])
;;

let () = Bonsai_web.Start.start component
