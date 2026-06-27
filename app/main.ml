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
    },
    {
      "capture_run_id": 43,
      "recorded_at": "2026-06-19T07:18:04Z",
      "session_name": "Morning form check",
      "has_warmup": false,
      "has_main": true,
      "warmup_sample_count": null,
      "main_sample_count": 920,
      "model_name": "hsmm-phase",
      "model_version": "2026-06-18",
      "labels_present": false,
      "analysis_present": true
    }
  ]
}
|}
;;

let capture_index_model =
  match Labeller.Bundle_manifest.parse_string sample_manifest_json with
  | Ok manifest ->
    Labeller.Model.empty
    |> Fn.flip Labeller.Model.apply_exn (Labeller.Load_manifest manifest)
    |> Fn.flip Labeller.Model.apply_exn (Labeller.Select_capture (Labeller.Capture_id.of_int 42))
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

let sample_count_text capture segment =
  Labeller.Capture_metadata.sample_count capture segment
  |> Option.value_map ~default:"—" ~f:(fun count -> [%string "%{count#Int} samples"])
;;

let primary_segment capture =
  if Labeller.Capture_metadata.has_segment capture Labeller.Segment.Main
  then Some Labeller.Segment.Main
  else if Labeller.Capture_metadata.has_segment capture Labeller.Segment.Warmup
  then Some Labeller.Segment.Warmup
  else None
;;

let segment_name = function
  | Labeller.Segment.Warmup -> "Warmup"
  | Main -> "Main"
;;

let trace_summary capture =
  match primary_segment capture with
  | None -> "No trace data"
  | Some segment -> [%string "%{segment_name segment} trace · %{sample_count_text capture segment}"]
;;

let status_label = function
  | Labeller.Capture_metadata.Needs_labels -> "Needs labels"
  | Ready_to_review -> "Ready to review"
  | Analysis_missing -> "Analysis missing"
  | No_trace_data -> "No trace data"
;;

let next_action_text capture =
  match Labeller.Capture_metadata.operational_status capture, primary_segment capture with
  | No_trace_data, _ -> "Next: export or restore trace data"
  | Analysis_missing, _ -> "Next: run pose analysis"
  | Needs_labels, Some segment -> [%string "Next: label %{segment_name segment} segment"]
  | Needs_labels, None -> "Next: choose a trace to label"
  | Ready_to_review, Some segment -> [%string "Next: review %{segment_name segment} labels"]
  | Ready_to_review, None -> "Next: review labels"
;;

let status_detail_text capture =
  match Labeller.Capture_metadata.operational_status capture with
  | No_trace_data -> "Trace missing · labelling unavailable"
  | Analysis_missing -> [%string "%{trace_summary capture} · analysis missing"]
  | Needs_labels -> [%string "%{trace_summary capture} · labels missing"]
  | Ready_to_review -> [%string "%{trace_summary capture} · labels ready"]
;;

let pill text =
  Vdom.Node.span
    ~attrs:
      [ style
          "display:inline-flex;align-items:center;height:22px;padding:0 8px;border:1px solid #dedbd2;border-radius:999px;background:#fbfaf7;color:#6f6a60;font-size:12px;line-height:22px;"
      ]
    [ Vdom.Node.text text ]
;;

let status_badge status =
  let colors =
    match status with
    | Labeller.Capture_metadata.Needs_labels ->
      "background:#fff6df;color:#6c4f13;border-color:#ead8a6;"
    | Ready_to_review -> "background:#eef6ed;color:#405f3d;border-color:#d5e4d1;"
    | Analysis_missing -> "background:#f8eeee;color:#754a45;border-color:#ead3cf;"
    | No_trace_data -> "background:#f3f2ef;color:#6f6a60;border-color:#dfdcd4;"
  in
  Vdom.Node.span
    ~attrs:
      [ style
          [%string
            "display:inline-flex;align-items:center;height:22px;padding:0 8px;border:1px solid;border-radius:999px;font-size:12px;line-height:22px;white-space:nowrap;%{colors}"]
      ]
    [ Vdom.Node.text (status_label status) ]
;;

module Ui_model = struct
  type t = Labeller.Model.t [@@deriving sexp, equal]
end

module Ui_action = struct
  type t = Labeller.action [@@deriving sexp_of]
end

let apply_ui_action ~inject:_ ~schedule_event:_ model action =
  match Labeller.Model.apply model action with
  | Ok model -> model
  | Error _ -> model
;;

let capture_row ~selected_capture ~inject_action capture =
  let selected =
    Option.exists selected_capture ~f:(fun selected_capture ->
      Labeller.Capture_id.equal
        selected_capture
        (Labeller.Capture_metadata.id capture))
  in
  let row_style =
    if selected
    then
      "display:grid;grid-template-columns:1fr auto;gap:16px;align-items:center;padding:14px 12px;margin:0 -12px;border-bottom:1px solid #e3ded3;border-radius:8px;background:#f5f1e8;"
    else
      "display:grid;grid-template-columns:1fr auto;gap:16px;align-items:center;padding:14px 0;border-bottom:1px solid #ece9e2;"
  in
  Vdom.Node.button
    ~attrs:
      [ style
          [%string
            "width:100%;appearance:none;border:0;text-align:left;font:inherit;cursor:pointer;%{row_style}"]
      ; Vdom.Attr.create "type" "button"
      ; Vdom.Attr.create "aria-selected" (Bool.to_string selected)
      ; Vdom.Attr.on_click (fun _ ->
          inject_action (Labeller.Select_capture (Labeller.Capture_metadata.id capture)))
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
            ~attrs:[ style "font-size:12px;color:#6f6a60;" ]
            [ Vdom.Node.text (status_detail_text capture) ]
        ]
    ; Vdom.Node.div
        ~attrs:[ style "display:flex;gap:6px;justify-content:flex-end;" ]
        [ status_badge (Labeller.Capture_metadata.operational_status capture) ]
    ]
;;

let section_title = function
  | Labeller.Capture_metadata.Needs_attention -> "Needs attention"
  | Ready -> "Ready"
  | Blocked -> "Blocked"
;;

let section_hint = function
  | Labeller.Capture_metadata.Needs_attention -> "Captures with missing labels or analysis."
  | Ready -> "Captures with trace, analysis, and labels available."
  | Blocked -> "Captures that cannot be labelled until trace data is restored."
;;

let capture_section ~selected_capture ~inject_action group captures =
  match captures with
  | [] -> None
  | captures ->
    Some
      (Vdom.Node.section
         ~attrs:[ style "padding-top:16px;border-top:1px solid #ece9e2;" ]
         [ Vdom.Node.div
             ~attrs:
               [ style
                   "display:flex;align-items:baseline;justify-content:space-between;margin-bottom:2px;"
               ]
             [ Vdom.Node.h3
                 ~attrs:
                   [ style
                       "font-size:12px;line-height:1.2;margin:0;color:#3a3731;font-weight:620;text-transform:uppercase;letter-spacing:0.04em;"
                   ]
                 [ Vdom.Node.text (section_title group) ]
             ; Vdom.Node.div
                 ~attrs:[ style "font-size:12px;color:#6f6a60;" ]
                 [ let count = List.length captures in
                   let suffix = if count = 1 then "" else "s" in
                   Vdom.Node.text [%string "%{count#Int} capture%{suffix}"]
                 ]
             ]
         ; Vdom.Node.div
             ~attrs:[ style "font-size:12px;color:#6f6a60;margin-bottom:6px;" ]
             [ Vdom.Node.text (section_hint group) ]
         ; Vdom.Node.div
             (List.map captures ~f:(capture_row ~selected_capture ~inject_action))
         ])
;;

let grouped_capture_sections ~selected_capture ~inject_action captures =
  let sorted =
    List.sort captures ~compare:Labeller.Capture_metadata.compare_operational_priority
  in
  [ Labeller.Capture_metadata.Needs_attention; Ready; Blocked ]
  |> List.filter_map ~f:(fun group ->
    let captures =
      List.filter sorted ~f:(fun capture ->
        Labeller.Capture_metadata.equal_operational_group
          (Labeller.Capture_metadata.operational_group capture)
          group)
    in
    capture_section ~selected_capture ~inject_action group captures)
;;

let capture_index ~selected_capture ~inject_action captures =
  Vdom.Node.section
    ~attrs:
      [ style
          "background:#fffefc;border:1px solid #e7e3da;border-radius:10px;padding:22px 24px;box-shadow:0 1px 1px rgba(15,15,15,0.03);"
      ]
    [ Vdom.Node.div
        ~attrs:[ style "display:flex;align-items:flex-end;justify-content:space-between;margin-bottom:14px;" ]
        [ Vdom.Node.div
            [ Vdom.Node.div
                ~attrs:[ style "font-size:12px;color:#6f6a60;margin-bottom:6px;" ]
                [ Vdom.Node.text "Capture index" ]
            ; Vdom.Node.h2
                ~attrs:[ style "font-size:20px;line-height:1.2;margin:0;color:#25231f;font-weight:560;" ]
                [ Vdom.Node.text "Pose export bundle" ]
            ]
        ; Vdom.Node.div
            ~attrs:[ style "font-size:12px;color:#6f6a60;" ]
            [ let count = List.length captures in
              let suffix = if count = 1 then "" else "s" in
              Vdom.Node.text [%string "%{count#Int} capture%{suffix}"]
            ]
        ]
    ; (match captures with
       | [] ->
         Vdom.Node.div
           ~attrs:
             [ style
                 "padding:42px 12px;color:#6f6a60;font-size:14px;text-align:center;border-top:1px solid #ece9e2;"
             ]
           [ Vdom.Node.text "Open a bundle to see captured pose traces here." ]
       | captures ->
         Vdom.Node.div
           ~attrs:[ style "display:flex;flex-direction:column;gap:14px;" ]
           (grouped_capture_sections ~selected_capture ~inject_action captures))
    ]
;;

let metadata_row label value =
  Vdom.Node.div
    ~attrs:
      [ style
          "display:grid;grid-template-columns:104px 1fr;gap:12px;padding:9px 0;border-bottom:1px solid #ebe7df;font-size:13px;"
      ]
    [ Vdom.Node.div ~attrs:[ style "color:#6f6a60;" ] [ Vdom.Node.text label ]
    ; Vdom.Node.div ~attrs:[ style "color:#3a3731;" ] [ Vdom.Node.text value ]
    ]
;;

let workspace_preview selected_capture =
  Vdom.Node.section
    ~attrs:
      [ style
          "min-height:320px;background:#fbfaf7;border:1px solid #e7e3da;border-radius:10px;padding:22px 24px;"
      ]
    [ Vdom.Node.div
        ~attrs:[ style "font-size:12px;color:#6f6a60;margin-bottom:6px;" ]
        [ Vdom.Node.text "Workspace" ]
    ; (match selected_capture with
       | None ->
         Vdom.Node.div
           [ Vdom.Node.h2
               ~attrs:[ style "font-size:20px;line-height:1.2;margin:0 0 14px;color:#25231f;font-weight:560;" ]
               [ Vdom.Node.text "Select a capture to label" ]
           ; Vdom.Node.p
               ~attrs:[ style "max-width:420px;margin:0;color:#6f6a60;font-size:14px;line-height:1.65;" ]
               [ Vdom.Node.text
                   "The replay canvas, timeline, and interval editor will live here. For now this shell proves the capture index can render bundle metadata in the app."
               ]
           ]
       | Some capture ->
         Vdom.Node.div
           [ Vdom.Node.h2
               ~attrs:
                 [ style
                     "font-size:20px;line-height:1.2;margin:0 0 14px;color:#25231f;font-weight:560;"
                 ]
               [ Vdom.Node.text (text_or_dash (Labeller.Capture_metadata.session_name capture)) ]
           ; Vdom.Node.div
               ~attrs:[ style "margin:18px 0 20px;" ]
               [ metadata_row
                   "State"
                   (status_label (Labeller.Capture_metadata.operational_status capture))
               ; metadata_row "Trace" (trace_summary capture)
               ; metadata_row
                   "Labels"
                   (if Labeller.Capture_metadata.labels_present capture then "Present" else "Missing")
               ; metadata_row
                   "Analysis"
                   (if Labeller.Capture_metadata.analysis_present capture then "Present" else "Missing")
               ]
           ; Vdom.Node.p
               ~attrs:[ style "margin:0;color:#3a3731;font-size:14px;line-height:1.6;font-weight:520;" ]
               [ Vdom.Node.text (next_action_text capture) ]
           ])
    ]
;;

let component =
  let open Bonsai.Let_syntax in
  let%sub model, inject_action =
    Bonsai.state_machine0
      (module Ui_model)
      (module Ui_action)
      ~default_model:capture_index_model
      ~apply_action:apply_ui_action
  in
  let%arr model = model
  and inject_action = inject_action in
  let captures = Labeller.Model.captures model in
  let selected_capture_id = Labeller.Model.selected_capture model in
  let selected_capture = Labeller.Model.selected_capture_metadata model in
  Vdom.Node.div
       ~attrs:
         [ style
             "min-height:100vh;background:#f7f5f0;color:#25231f;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;"
         ]
       [ Vdom.Node.main
           ~attrs:[ style "max-width:1120px;margin:0 auto;padding:56px 28px 72px;" ]
           [ Vdom.Node.header
               ~attrs:[ style "margin-bottom:28px;" ]
               [ Vdom.Node.div
                   ~attrs:[ style "font-size:13px;color:#6f6a60;margin-bottom:8px;" ]
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
               [ capture_index ~selected_capture:selected_capture_id ~inject_action captures
               ; workspace_preview selected_capture
               ]
           ]
       ]
;;

let () = Bonsai_web.Start.start component
