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

let with_temp_bundle_dir ~f =
  let dir = Core_unix.mkdtemp "burpee-pose-labeller-test-XXXXXX" in
  Exn.protect
    ~f:(fun () -> f dir)
    ~finally:(fun () ->
      let manifest_path = Filename.concat dir "manifest.json" in
      let labels_dir = Filename.concat dir "labels" in
      let label_path = Filename.concat labels_dir "capture-42-labels.json" in
      if Sys_unix.file_exists_exn manifest_path then Core_unix.unlink manifest_path;
      if Sys_unix.file_exists_exn label_path then Core_unix.unlink label_path;
      if Sys_unix.file_exists_exn labels_dir then Core_unix.rmdir labels_dir;
      Core_unix.rmdir dir)
;;

let test_manifest_parse_rejects_missing_captures () =
  let open Burpee_pose_labeller in
  let error = require_error (Bundle_manifest.parse_string {| { "version": 1 } |}) in
  match error with
  | Error.Invalid_manifest _ -> ()
  | _ -> raise_s [%message "expected Invalid_manifest" (error : Error.t)]
;;

let test_manifest_load_reads_manifest_json_from_bundle_dir () =
  let open Burpee_pose_labeller in
  with_temp_bundle_dir ~f:(fun dir ->
    Out_channel.write_all (Filename.concat dir "manifest.json") ~data:sample_manifest_json;
    let manifest = require_ok (Bundle_manifest.load ~bundle_dir:dir) in
    [%test_result: int] (List.length (Bundle_manifest.captures manifest)) ~expect:1)
;;

let test_manifest_load_reports_missing_manifest_file () =
  let open Burpee_pose_labeller in
  with_temp_bundle_dir ~f:(fun dir ->
    let error = require_error (Bundle_manifest.load ~bundle_dir:dir) in
    match error with
    | Error.Manifest_file_error _ -> ()
    | _ -> raise_s [%message "expected Manifest_file_error" (error : Error.t)])
;;

let sample_labels_json =
  {|
[
  {
    "source_capture_run_id": 42,
    "segment": "main",
    "start_ms": 10000,
    "end_ms": 13200,
    "label_type": "phase",
    "label": "descending",
    "source": "manual",
    "metadata": {}
  }
]
|}
;;

let test_label_store_parse_reads_bundle_label_json () =
  let open Burpee_pose_labeller in
  let labels = require_ok (Label_store.parse_string sample_labels_json) in
  [%test_result: int] (List.length labels) ~expect:1;
  let label = List.hd_exn labels in
  [%test_result: Label_id.t] (Label.id label) ~expect:(Label_id.of_int 1);
  [%test_result: Capture_id.t] (Label.capture_id label) ~expect:(Capture_id.of_int 42);
  [%test_result: Segment.t] (Label.segment label) ~expect:Segment.Main;
  [%test_result: int] (Interval.start_ms (Label.interval label)) ~expect:10000;
  [%test_result: int] (Interval.end_ms (Label.interval label)) ~expect:13200;
  [%test_result: Label_type.t] (Label.label_type label) ~expect:Label_type.Phase;
  [%test_result: string] (Label.label label) ~expect:"descending"
;;

let test_label_store_to_string_writes_bundle_label_json () =
  let open Burpee_pose_labeller in
  let interval = require_ok (Interval.create ~start_ms:10000 ~end_ms:13200) in
  let label =
    require_ok
      (Label.create
         ~id:(Label_id.of_int 7)
         ~capture_id:(Capture_id.of_int 42)
         ~segment:Segment.Main
         ~interval
         ~label_type:Label_type.Phase
         ~label:"descending")
  in
  let json = Yojson.Safe.from_string (Label_store.to_string [ label ]) in
  match json with
  | `List [ `Assoc fields ] ->
    let field_as_string name =
      Option.map (List.Assoc.find fields name ~equal:String.equal) ~f:Yojson.Safe.to_string
    in
    [%test_result: string option]
      (field_as_string "source_capture_run_id")
      ~expect:(Some "42");
    [%test_result: string option] (field_as_string "segment") ~expect:(Some "\"main\"");
    [%test_result: string option] (field_as_string "source") ~expect:(Some "\"manual\"");
    [%test_result: string option] (field_as_string "metadata") ~expect:(Some "{}")
  | _ -> raise_s [%message "expected a single label object" ~json:(Yojson.Safe.to_string json : string)]
;;

let test_label_store_parse_rejects_unknown_label_type () =
  let open Burpee_pose_labeller in
  let error =
    require_error
      (Label_store.parse_string
         {|[{"source_capture_run_id":42,"segment":"main","start_ms":1,"end_ms":2,"label_type":"nonsense","label":"x","source":"manual","metadata":{}}]|})
  in
  match error with
  | Error.Invalid_labels _ -> ()
  | _ -> raise_s [%message "expected Invalid_labels" (error : Error.t)]
;;

let test_label_store_save_and_load_json_file_round_trips_labels () =
  let open Burpee_pose_labeller in
  with_temp_bundle_dir ~f:(fun dir ->
    let labels_dir = Filename.concat dir "labels" in
    Core_unix.mkdir labels_dir;
    let path = Filename.concat labels_dir "capture-42-labels.json" in
    let interval = require_ok (Interval.create ~start_ms:10000 ~end_ms:13200) in
    let label =
      require_ok
        (Label.create
           ~id:(Label_id.of_int 9)
           ~capture_id:(Capture_id.of_int 42)
           ~segment:Segment.Main
           ~interval
           ~label_type:Label_type.Phase
           ~label:"descending")
    in
    require_ok (Label_store.save_json_file ~path [ label ]);
    let loaded = require_ok (Label_store.load_json_file ~path) in
    [%test_result: int] (List.length loaded) ~expect:1;
    [%test_result: string] (Label.label (List.hd_exn loaded)) ~expect:"descending")
;;

let test_label_store_load_json_file_reports_missing_file () =
  let open Burpee_pose_labeller in
  with_temp_bundle_dir ~f:(fun dir ->
    let error =
      require_error (Label_store.load_json_file ~path:(Filename.concat dir "missing-labels.json"))
    in
    match error with
    | Error.Label_file_error _ -> ()
    | _ -> raise_s [%message "expected Label_file_error" (error : Error.t)])
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
  test_manifest_load_reads_manifest_json_from_bundle_dir ();
  test_manifest_load_reports_missing_manifest_file ();
  test_label_store_parse_reads_bundle_label_json ();
  test_label_store_to_string_writes_bundle_label_json ();
  test_label_store_parse_rejects_unknown_label_type ();
  test_label_store_save_and_load_json_file_round_trips_labels ();
  test_label_store_load_json_file_reports_missing_file ();
  test_interval_requires_end_after_start ();
  test_ending_an_interval_stores_a_draft_interval ();
  test_adding_a_label_stores_it_and_marks_the_model_dirty ();
  test_deleting_a_label_removes_it_and_marks_the_model_dirty ();
  test_editing_a_label_replaces_the_matching_id_and_marks_the_model_dirty ();
  test_marking_saved_clears_unsaved_changes ()
;;
