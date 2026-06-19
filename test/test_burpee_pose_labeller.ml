open! Core

let require_ok = function
  | Ok value -> value
  | Error error -> raise_s [%message "expected Ok" (error : Burpee_pose_labeller.Error.t)]
;;

let require_error = function
  | Ok _ -> raise_s [%message "expected Error"]
  | Error error -> error
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
  test_interval_requires_end_after_start ();
  test_ending_an_interval_stores_a_draft_interval ();
  test_adding_a_label_stores_it_and_marks_the_model_dirty ();
  test_deleting_a_label_removes_it_and_marks_the_model_dirty ();
  test_editing_a_label_replaces_the_matching_id_and_marks_the_model_dirty ();
  test_marking_saved_clears_unsaved_changes ()
;;
