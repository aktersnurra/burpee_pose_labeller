open! Core

let require_ok = function
  | Ok value -> value
  | Error error -> raise_s [%message "expected Ok" (error : Burpee_pose_labeller.Error.t)]
;;

let require_error = function
  | Ok _ -> raise_s [%message "expected Error"]
  | Error error -> error
;;

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
      "labels_present": false,
      "analysis_present": true
    }
  ]
}
|}
;;

let test_manifest_parse_exposes_capture_index_metadata () =
  let open Burpee_pose_labeller in
  let manifest = require_ok (Bundle_manifest.parse_string sample_manifest_json) in
  let captures = Bundle_manifest.captures manifest in
  [%test_result: int] (List.length captures) ~expect:1;
  let capture = List.hd_exn captures in
  [%test_result: Capture_id.t]
    (Capture_metadata.id capture)
    ~expect:(Capture_id.of_int 42);
  [%test_result: string option]
    (Capture_metadata.session_name capture)
    ~expect:(Some "Lunch burpees");
  [%test_result: bool] (Capture_metadata.has_segment capture Segment.Warmup) ~expect:true;
  [%test_result: bool] (Capture_metadata.has_segment capture Segment.Main) ~expect:true;
  [%test_result: int option]
    (Capture_metadata.sample_count capture Segment.Main)
    ~expect:(Some 1800);
  [%test_result: string option]
    (Capture_metadata.model_version capture)
    ~expect:(Some "2026-06-18");
  [%test_result: bool] (Capture_metadata.labels_present capture) ~expect:false;
  [%test_result: bool] (Capture_metadata.analysis_present capture) ~expect:true
;;

let test_manifest_parse_rejects_missing_captures () =
  let open Burpee_pose_labeller in
  let error = require_error (Bundle_manifest.parse_string {| { "version": 1 } |}) in
  match error with
  | Error.Invalid_manifest _ -> ()
  | _ -> raise_s [%message "expected Invalid_manifest" (error : Error.t)]
;;

let test_interval_requires_end_after_start () =
  let open Burpee_pose_labeller in
  let _ = require_ok (Interval.create ~start_ms:10 ~end_ms:20) in
  let error = require_error (Interval.create ~start_ms:20 ~end_ms:20) in
  [%test_result: Error.t]
    error
    ~expect:(Error.Invalid_interval { start_ms = 20; end_ms = 20 })
;;

let test_ending_an_interval_stores_a_draft_interval () =
  let open Burpee_pose_labeller in
  let model = Model.apply_exn Model.empty (Start_interval 1_000) in
  let model = Model.apply_exn model (End_interval 2_500) in
  [%test_result: Interval.t option]
    (Model.draft_interval model)
    ~expect:(Some (require_ok (Interval.create ~start_ms:1_000 ~end_ms:2_500)))
;;

let test_adding_a_label_stores_it_and_marks_the_model_dirty () =
  let open Burpee_pose_labeller in
  let interval = require_ok (Interval.create ~start_ms:1_000 ~end_ms:2_500) in
  let label =
    require_ok
      (Label.create
         ~id:(Label_id.of_int 1)
         ~capture_id:(Capture_id.of_int 42)
         ~segment:Segment.Main
         ~interval
         ~label_type:Label_type.Phase
         ~label:"descending")
  in
  let model = Model.apply_exn Model.empty (Add_label label) in
  [%test_result: int] (List.length (Model.labels model)) ~expect:1;
  [%test_result: bool] (Model.has_unsaved_changes model) ~expect:true
;;

let test_deleting_a_label_removes_it_and_marks_the_model_dirty () =
  let open Burpee_pose_labeller in
  let interval = require_ok (Interval.create ~start_ms:1_000 ~end_ms:2_500) in
  let label =
    require_ok
      (Label.create
         ~id:(Label_id.of_int 1)
         ~capture_id:(Capture_id.of_int 42)
         ~segment:Segment.Main
         ~interval
         ~label_type:Label_type.Phase
         ~label:"descending")
  in
  let model = Model.apply_exn Model.empty (Add_label label) in
  let model = Model.apply_exn model (Delete_label (Label_id.of_int 1)) in
  [%test_result: int] (List.length (Model.labels model)) ~expect:0;
  [%test_result: bool] (Model.has_unsaved_changes model) ~expect:true
;;

let test_editing_a_label_replaces_the_matching_id_and_marks_the_model_dirty () =
  let open Burpee_pose_labeller in
  let original_interval = require_ok (Interval.create ~start_ms:1_000 ~end_ms:2_500) in
  let edited_interval = require_ok (Interval.create ~start_ms:1_100 ~end_ms:2_600) in
  let original =
    require_ok
      (Label.create
         ~id:(Label_id.of_int 1)
         ~capture_id:(Capture_id.of_int 42)
         ~segment:Segment.Main
         ~interval:original_interval
         ~label_type:Label_type.Phase
         ~label:"descending")
  in
  let edited =
    require_ok
      (Label.create
         ~id:(Label_id.of_int 1)
         ~capture_id:(Capture_id.of_int 42)
         ~segment:Segment.Main
         ~interval:edited_interval
         ~label_type:Label_type.Phase
         ~label:"bottom")
  in
  let model = Model.apply_exn Model.empty (Add_label original) in
  let model = Model.apply_exn model Mark_saved in
  let model = Model.apply_exn model (Edit_label edited) in
  [%test_result: Label.t list] (Model.labels model) ~expect:[ edited ];
  [%test_result: bool] (Model.has_unsaved_changes model) ~expect:true
;;

let test_marking_saved_clears_unsaved_changes () =
  let open Burpee_pose_labeller in
  let interval = require_ok (Interval.create ~start_ms:1_000 ~end_ms:2_500) in
  let label =
    require_ok
      (Label.create
         ~id:(Label_id.of_int 1)
         ~capture_id:(Capture_id.of_int 42)
         ~segment:Segment.Main
         ~interval
         ~label_type:Label_type.Phase
         ~label:"descending")
  in
  let model = Model.apply_exn Model.empty (Add_label label) in
  let model = Model.apply_exn model Mark_saved in
  [%test_result: bool] (Model.has_unsaved_changes model) ~expect:false
;;

let () =
  test_manifest_parse_exposes_capture_index_metadata ();
  test_manifest_parse_rejects_missing_captures ();
  test_interval_requires_end_after_start ();
  test_ending_an_interval_stores_a_draft_interval ();
  test_adding_a_label_stores_it_and_marks_the_model_dirty ();
  test_deleting_a_label_removes_it_and_marks_the_model_dirty ();
  test_editing_a_label_replaces_the_matching_id_and_marks_the_model_dirty ();
  test_marking_saved_clears_unsaved_changes ()
;;
