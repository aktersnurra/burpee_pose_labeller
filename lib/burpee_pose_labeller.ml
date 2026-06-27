open! Core

module Error = struct
  type t =
    | Invalid_interval of
        { start_ms : int
        ; end_ms : int
        }
    | Empty_label
    | No_interval_in_progress
    | Invalid_manifest of string
    | Invalid_labels of string
    | Invalid_trace of string
    | Capture_not_found of int
    | Manifest_file_error of
        { path : string
        ; message : string
        }
    | Label_file_error of
        { path : string
        ; message : string
        }
    | Trace_file_error of
        { path : string
        ; message : string
        }
  [@@deriving compare, equal, sexp]
end

module Capture_id = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp]
  end

  include T

  let of_int t = t
  let to_int t = t
end

module Label_id = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp]
  end

  include T

  let of_int t = t
  let to_int t = t
end

module Segment = struct
  type t =
    | Warmup
    | Main
  [@@deriving compare, equal, sexp]
end

module Label_type = struct
  type t =
    | Phase
    | Rep
    | Quality
    | Tag
  [@@deriving compare, equal, sexp]
end

module Capture_metadata = struct
  type operational_status =
    | Needs_labels
    | Ready_to_review
    | Analysis_missing
    | No_trace_data
  [@@deriving compare, equal, sexp]

  type operational_group =
    | Needs_attention
    | Ready
    | Blocked
  [@@deriving compare, equal, sexp]

  type t =
    { id : Capture_id.t
    ; recorded_at : string option
    ; session_name : string option
    ; has_warmup : bool
    ; has_main : bool
    ; warmup_sample_count : int option
    ; main_sample_count : int option
    ; model_name : string option
    ; model_version : string option
    ; labels_present : bool
    ; analysis_present : bool
    }
  [@@deriving compare, equal, sexp]

  let id t = t.id
  let recorded_at t = t.recorded_at
  let session_name t = t.session_name

  let has_segment t = function
    | Segment.Warmup -> t.has_warmup
    | Main -> t.has_main
  ;;

  let sample_count t = function
    | Segment.Warmup -> t.warmup_sample_count
    | Main -> t.main_sample_count
  ;;

  let model_name t = t.model_name
  let model_version t = t.model_version
  let labels_present t = t.labels_present
  let analysis_present t = t.analysis_present

  let has_any_trace t = t.has_warmup || t.has_main

  let operational_status t =
    if not (has_any_trace t)
    then No_trace_data
    else if not t.analysis_present
    then Analysis_missing
    else if not t.labels_present
    then Needs_labels
    else Ready_to_review
  ;;

  let operational_rank t =
    match operational_status t with
    | Needs_labels -> 0
    | Analysis_missing -> 1
    | Ready_to_review -> 2
    | No_trace_data -> 3
  ;;

  let compare_operational_priority left right =
    Int.compare (operational_rank left) (operational_rank right)
  ;;

  let operational_group t =
    match operational_status t with
    | Needs_labels | Analysis_missing -> Needs_attention
    | Ready_to_review -> Ready
    | No_trace_data -> Blocked
  ;;
end

module Bundle_manifest = struct
  type t = { captures : Capture_metadata.t list } [@@deriving compare, equal, sexp]

  let invalid message = Error (Error.Invalid_manifest message)

  let field json name =
    match json with
    | `Assoc fields -> List.Assoc.find fields name ~equal:String.equal
    | _ -> None
  ;;

  let required_int json name =
    match field json name with
    | Some (`Int value) -> Ok value
    | Some _ -> invalid [%string "field %{name} must be an integer"]
    | None -> invalid [%string "missing field %{name}"]
  ;;

  let optional_int json name =
    match field json name with
    | Some (`Int value) -> Ok (Some value)
    | Some `Null | None -> Ok None
    | Some _ -> invalid [%string "field %{name} must be an integer"]
  ;;

  let optional_string json name =
    match field json name with
    | Some (`String value) -> Ok (Some value)
    | Some `Null | None -> Ok None
    | Some _ -> invalid [%string "field %{name} must be a string"]
  ;;

  let optional_bool json name =
    match field json name with
    | Some (`Bool value) -> Ok value
    | Some `Null | None -> Ok false
    | Some _ -> invalid [%string "field %{name} must be a boolean"]
  ;;

  let parse_capture json =
    let open Result.Let_syntax in
    let%bind id = required_int json "capture_run_id" in
    let%bind recorded_at = optional_string json "recorded_at" in
    let%bind session_name = optional_string json "session_name" in
    let%bind has_warmup = optional_bool json "has_warmup" in
    let%bind has_main = optional_bool json "has_main" in
    let%bind warmup_sample_count = optional_int json "warmup_sample_count" in
    let%bind main_sample_count = optional_int json "main_sample_count" in
    let%bind model_name = optional_string json "model_name" in
    let%bind model_version = optional_string json "model_version" in
    let%bind labels_present = optional_bool json "labels_present" in
    let%map analysis_present = optional_bool json "analysis_present" in
    { Capture_metadata.id = Capture_id.of_int id
    ; recorded_at
    ; session_name
    ; has_warmup
    ; has_main
    ; warmup_sample_count
    ; main_sample_count
    ; model_name
    ; model_version
    ; labels_present
    ; analysis_present
    }
  ;;

  let parse_json json =
    match field json "captures" with
    | Some (`List captures) ->
      let open Result.Let_syntax in
      let%map captures = Result.all (List.map captures ~f:parse_capture) in
      { captures }
    | Some _ -> invalid "field captures must be an array"
    | None -> invalid "missing field captures"
  ;;

  let parse_string string =
    match Yojson.Safe.from_string string with
    | json -> parse_json json
    | exception Yojson.Json_error message -> invalid message
  ;;

  let load ~bundle_dir =
    let path = Filename.concat bundle_dir "manifest.json" in
    match In_channel.read_all path with
    | contents -> parse_string contents
    | exception exn ->
      Error (Error.Manifest_file_error { path; message = Exn.to_string exn })
  ;;

  let captures t = t.captures
end

module Bundle_paths = struct
  let segment_filename_part = function
    | Segment.Warmup -> "warmup"
    | Main -> "main"
  ;;

  let trace_json_file ~bundle_dir ~capture_id ~segment =
    Filename.concat
      (Filename.concat bundle_dir "traces")
      [%string
        "capture-%{Capture_id.to_int capture_id#Int}-%{segment_filename_part segment}.json.zst"]
  ;;

  let labels_json_file ~bundle_dir ~capture_id =
    Filename.concat
      (Filename.concat bundle_dir "labels")
      [%string "capture-%{Capture_id.to_int capture_id#Int}-labels.json.zst"]
  ;;
end

let is_zstd_path path = String.is_suffix path ~suffix:".zst"

let run_command command =
  match Stdlib.Sys.command command with
  | 0 -> Ok ()
  | exit_code -> Error [%string "command failed with exit code %{exit_code#Int}"]
;;

let read_text_file ~path ~on_error =
  if is_zstd_path path
  then
    if not (Stdlib.Sys.file_exists path)
    then Error (on_error ~path ~message:"No such file or directory")
    else (
      let temp_path = Stdlib.Filename.temp_file "burpee-pose-labeller-" ".json" in
      let command =
        [%string
          "zstd -q -d -f %{Stdlib.Filename.quote path} -o %{Stdlib.Filename.quote temp_path}"]
      in
      match run_command command with
      | Error message ->
        (match Stdlib.Sys.remove temp_path with
         | () -> ()
         | exception _ -> ());
        Error (on_error ~path ~message)
      | Ok () ->
        (match In_channel.read_all temp_path with
         | contents ->
           Stdlib.Sys.remove temp_path;
           Ok contents
         | exception exn ->
           (match Stdlib.Sys.remove temp_path with
            | () -> ()
            | exception _ -> ());
           Error (on_error ~path ~message:(Exn.to_string exn))))
  else
    match In_channel.read_all path with
    | contents -> Ok contents
    | exception exn -> Error (on_error ~path ~message:(Exn.to_string exn))
;;

let write_text_file ~path ~data ~on_error =
  if is_zstd_path path
  then (
    let temp_path = Stdlib.Filename.temp_file "burpee-pose-labeller-" ".json" in
    match Out_channel.write_all temp_path ~data with
    | exception exn ->
      (match Stdlib.Sys.remove temp_path with
       | () -> ()
       | exception _ -> ());
      Error (on_error ~path ~message:(Exn.to_string exn))
    | () ->
      let command =
        [%string
          "zstd -q -f %{Stdlib.Filename.quote temp_path} -o %{Stdlib.Filename.quote path}"]
      in
      (match run_command command with
       | Ok () ->
         Stdlib.Sys.remove temp_path;
         Ok ()
       | Error message ->
         (match Stdlib.Sys.remove temp_path with
          | () -> ()
          | exception _ -> ());
         Error (on_error ~path ~message)))
  else
    match Out_channel.write_all path ~data with
    | () -> Ok ()
    | exception exn -> Error (on_error ~path ~message:(Exn.to_string exn))
;;

module Interval = struct
  type t =
    { start_ms : int
    ; end_ms : int
    }
  [@@deriving compare, equal, sexp]

  let create ~start_ms ~end_ms =
    if end_ms > start_ms
    then Ok { start_ms; end_ms }
    else Error (Error.Invalid_interval { start_ms; end_ms })
  ;;

  let start_ms t = t.start_ms
  let end_ms t = t.end_ms
end

module Label = struct
  type t =
    { id : Label_id.t
    ; capture_id : Capture_id.t
    ; segment : Segment.t
    ; interval : Interval.t
    ; label_type : Label_type.t
    ; label : string
    }
  [@@deriving compare, equal, sexp]

  let create ~id ~capture_id ~segment ~interval ~label_type ~label =
    if String.is_empty (String.strip label)
    then Error Error.Empty_label
    else Ok { id; capture_id; segment; interval; label_type; label }
  ;;

  let id t = t.id
  let capture_id t = t.capture_id
  let segment t = t.segment
  let interval t = t.interval
  let label_type t = t.label_type
  let label t = t.label
end

let json_field json name =
  match json with
  | `Assoc fields -> List.Assoc.find fields name ~equal:String.equal
  | _ -> None
;;

module Label_store = struct
  let invalid message = Error (Error.Invalid_labels message)

  let field json name =
    match json with
    | `Assoc fields -> List.Assoc.find fields name ~equal:String.equal
    | _ -> None
  ;;

  let required_int json name =
    match field json name with
    | Some (`Int value) -> Ok value
    | Some _ -> invalid [%string "field %{name} must be an integer"]
    | None -> invalid [%string "missing field %{name}"]
  ;;

  let required_string json name =
    match json_field json name with
    | Some (`String value) -> Ok value
    | Some _ -> invalid [%string "field %{name} must be a string"]
    | None -> invalid [%string "missing field %{name}"]
  ;;

  let segment_of_string = function
    | "warmup" -> Ok Segment.Warmup
    | "main" -> Ok Main
    | value -> invalid [%string "unknown segment %{value}"]
  ;;

  let segment_to_string = function
    | Segment.Warmup -> "warmup"
    | Main -> "main"
  ;;

  let label_type_of_string = function
    | "phase" -> Ok Label_type.Phase
    | "rep" -> Ok Rep
    | "quality" -> Ok Quality
    | "tag" -> Ok Tag
    | value -> invalid [%string "unknown label_type %{value}"]
  ;;

  let label_type_to_string = function
    | Label_type.Phase -> "phase"
    | Rep -> "rep"
    | Quality -> "quality"
    | Tag -> "tag"
  ;;

  let parse_label ~index json =
    let open Result.Let_syntax in
    let%bind capture_id = required_int json "source_capture_run_id" in
    let%bind segment_string = required_string json "segment" in
    let%bind segment = segment_of_string segment_string in
    let%bind start_ms = required_int json "start_ms" in
    let%bind end_ms = required_int json "end_ms" in
    let%bind interval = Interval.create ~start_ms ~end_ms in
    let%bind label_type_string = required_string json "label_type" in
    let%bind label_type = label_type_of_string label_type_string in
    let%bind label = required_string json "label" in
    Label.create
      ~id:(Label_id.of_int (index + 1))
      ~capture_id:(Capture_id.of_int capture_id)
      ~segment
      ~interval
      ~label_type
      ~label
  ;;

  let parse_json = function
    | `List labels -> Result.all (List.mapi labels ~f:(fun index json -> parse_label ~index json))
    | _ -> invalid "labels file must be a JSON array"
  ;;

  let parse_string string =
    match Yojson.Safe.from_string string with
    | json -> parse_json json
    | exception Yojson.Json_error message -> invalid message
  ;;

  let label_to_json label =
    `Assoc
      [ "source_capture_run_id", `Int (Capture_id.to_int (Label.capture_id label))
      ; "segment", `String (segment_to_string (Label.segment label))
      ; "start_ms", `Int (Interval.start_ms (Label.interval label))
      ; "end_ms", `Int (Interval.end_ms (Label.interval label))
      ; "label_type", `String (label_type_to_string (Label.label_type label))
      ; "label", `String (Label.label label)
      ; "source", `String "manual"
      ; "metadata", `Assoc []
      ]
  ;;

  let to_string labels = Yojson.Safe.to_string (`List (List.map labels ~f:label_to_json))

  let load_json_file ~path =
    let on_error ~path ~message = Error.Label_file_error { path; message } in
    let open Result.Let_syntax in
    let%bind contents = read_text_file ~path ~on_error in
    parse_string contents
  ;;

  let save_json_file ~path labels =
    let on_error ~path ~message = Error.Label_file_error { path; message } in
    write_text_file ~path ~data:(to_string labels) ~on_error
  ;;
end

let is_missing_file_error message =
  String.is_substring message ~substring:"No such file"
  || String.is_substring message ~substring:"ENOENT"
;;

module Trace = struct
  module Keypoint = struct
    type t =
      { name : string
      ; x : float
      ; y : float
      ; score : float option
      }
    [@@deriving compare, equal, sexp]

    let name t = t.name
    let x t = t.x
    let y t = t.y
    let score t = t.score
  end

  module Sample = struct
    type t =
      { time_ms : int
      ; keypoints : Keypoint.t list
      }
    [@@deriving compare, equal, sexp]

    let time_ms t = t.time_ms
    let keypoints t = t.keypoints
  end

  type t = { samples : Sample.t list } [@@deriving compare, equal, sexp]

  let invalid message = Error (Error.Invalid_trace message)

  let required_int json name =
    match json_field json name with
    | Some (`Int value) -> Ok value
    | Some _ -> invalid [%string "field %{name} must be an integer"]
    | None -> invalid [%string "missing field %{name}"]
  ;;

  let required_string json name =
    match json_field json name with
    | Some (`String value) -> Ok value
    | Some _ -> invalid [%string "field %{name} must be a string"]
    | None -> invalid [%string "missing field %{name}"]
  ;;

  let required_float json name =
    match json_field json name with
    | Some (`Float value) -> Ok value
    | Some (`Int value) -> Ok (Float.of_int value)
    | Some _ -> invalid [%string "field %{name} must be a number"]
    | None -> invalid [%string "missing field %{name}"]
  ;;

  let optional_float json name =
    match json_field json name with
    | Some (`Float value) -> Ok (Some value)
    | Some (`Int value) -> Ok (Some (Float.of_int value))
    | Some `Null | None -> Ok None
    | Some _ -> invalid [%string "field %{name} must be a number"]
  ;;

  let parse_keypoint json =
    let open Result.Let_syntax in
    let%bind name = required_string json "name" in
    let%bind x = required_float json "x" in
    let%bind y = required_float json "y" in
    let%map score = optional_float json "score" in
    { Keypoint.name; x; y; score }
  ;;

  let parse_sample json =
    let open Result.Let_syntax in
    let%bind time_ms = required_int json "time_ms" in
    match json_field json "keypoints" with
    | Some (`List keypoints) ->
      let%map keypoints = Result.all (List.map keypoints ~f:parse_keypoint) in
      { Sample.time_ms; keypoints }
    | Some _ -> invalid "field keypoints must be an array"
    | None -> invalid "missing field keypoints"
  ;;

  let parse_json = function
    | `List samples ->
      let open Result.Let_syntax in
      let%map samples = Result.all (List.map samples ~f:parse_sample) in
      { samples }
    | _ -> invalid "trace file must be a JSON array"
  ;;

  let parse_string string =
    match Yojson.Safe.from_string string with
    | json -> parse_json json
    | exception Yojson.Json_error message -> invalid message
  ;;

  let load_json_file ~path =
    let on_error ~path ~message = Error.Trace_file_error { path; message } in
    let open Result.Let_syntax in
    let%bind contents = read_text_file ~path ~on_error in
    parse_string contents
  ;;

  let samples t = t.samples
end

module Bundle_workspace = struct
  type t =
    { capture : Capture_metadata.t
    ; segment : Segment.t
    ; trace : Trace.t
    ; labels : Label.t list
    }
  [@@deriving compare, equal, sexp]

  let find_capture manifest capture_id =
    match
      List.find (Bundle_manifest.captures manifest) ~f:(fun capture ->
        Capture_id.equal (Capture_metadata.id capture) capture_id)
    with
    | Some capture -> Ok capture
    | None -> Error (Error.Capture_not_found (Capture_id.to_int capture_id))
  ;;

  let load_labels_or_empty ~path =
    match Label_store.load_json_file ~path with
    | Ok labels -> Ok labels
    | Error (Error.Label_file_error { message; _ }) when is_missing_file_error message -> Ok []
    | Error error -> Error error
  ;;

  let load ~bundle_dir ~capture_id ~segment =
    let open Result.Let_syntax in
    let%bind manifest = Bundle_manifest.load ~bundle_dir in
    let%bind capture = find_capture manifest capture_id in
    let%bind trace =
      Trace.load_json_file ~path:(Bundle_paths.trace_json_file ~bundle_dir ~capture_id ~segment)
    in
    let%map labels =
      load_labels_or_empty ~path:(Bundle_paths.labels_json_file ~bundle_dir ~capture_id)
    in
    { capture; segment; trace; labels }
  ;;

  let capture t = t.capture
  let segment t = t.segment
  let trace t = t.trace
  let labels t = t.labels
end

type action =
  | Select_capture of Capture_id.t
  | Select_segment of Segment.t
  | Seek of int
  | Start_interval of int
  | End_interval of int
  | Add_label of Label.t
  | Edit_label of Label.t
  | Load_manifest of Bundle_manifest.t
  | Load_workspace of Bundle_workspace.t
  | Delete_label of Label_id.t
  | Mark_saved
[@@deriving sexp]

module Model = struct
  type t =
    { captures : Capture_metadata.t list
    ; selected_capture : Capture_id.t option
    ; selected_segment : Segment.t
    ; current_time_ms : int
    ; interval_start_ms : int option
    ; draft_interval : Interval.t option
    ; loaded_trace : Trace.t option
    ; labels : Label.t list
    ; has_unsaved_changes : bool
    }
  [@@deriving sexp, equal]

  let empty =
    { captures = []
    ; selected_capture = None
    ; selected_segment = Main
    ; current_time_ms = 0
    ; interval_start_ms = None
    ; draft_interval = None
    ; loaded_trace = None
    ; labels = []
    ; has_unsaved_changes = false
    }
  ;;

  let capture_exists captures capture_id =
    List.exists captures ~f:(fun capture ->
      Capture_id.equal (Capture_metadata.id capture) capture_id)
  ;;

  let upsert_capture captures capture =
    let capture_id = Capture_metadata.id capture in
    if capture_exists captures capture_id
    then captures
    else capture :: captures
  ;;

  let apply t = function
    | Select_capture selected_capture ->
      if capture_exists t.captures selected_capture
      then Ok { t with selected_capture = Some selected_capture }
      else Error (Error.Capture_not_found (Capture_id.to_int selected_capture))
    | Select_segment selected_segment -> Ok { t with selected_segment }
    | Seek current_time_ms -> Ok { t with current_time_ms }
    | Start_interval start_ms -> Ok { t with interval_start_ms = Some start_ms; draft_interval = None }
    | End_interval end_ms ->
      (match t.interval_start_ms with
       | None -> Error Error.No_interval_in_progress
       | Some start_ms ->
         let%map.Result draft_interval = Interval.create ~start_ms ~end_ms in
         { t with interval_start_ms = None; draft_interval = Some draft_interval })
    | Add_label label ->
      Ok { t with labels = label :: t.labels; has_unsaved_changes = true }
    | Edit_label edited_label ->
      Ok
        { t with
          labels =
            List.map t.labels ~f:(fun label ->
              if Label_id.equal (Label.id label) (Label.id edited_label) then edited_label else label)
        ; has_unsaved_changes = true
        }
    | Load_manifest manifest -> Ok { t with captures = Bundle_manifest.captures manifest }
    | Load_workspace workspace ->
      let capture = Bundle_workspace.capture workspace in
      Ok
        { captures = upsert_capture t.captures capture
        ; selected_capture = Some (Capture_metadata.id capture)
        ; selected_segment = Bundle_workspace.segment workspace
        ; current_time_ms = 0
        ; interval_start_ms = None
        ; draft_interval = None
        ; loaded_trace = Some (Bundle_workspace.trace workspace)
        ; labels = List.rev (Bundle_workspace.labels workspace)
        ; has_unsaved_changes = false
        }
    | Delete_label label_id ->
      Ok
        { t with
          labels = List.filter t.labels ~f:(fun label -> not (Label_id.equal (Label.id label) label_id))
        ; has_unsaved_changes = true
        }
    | Mark_saved -> Ok { t with has_unsaved_changes = false }
  ;;

  let apply_exn t action =
    match apply t action with
    | Ok t -> t
    | Error error -> raise_s [%message "invalid model action" (error : Error.t)]
  ;;

  let captures t = t.captures
  let selected_capture t = t.selected_capture

  let selected_capture_metadata t =
    Option.bind t.selected_capture ~f:(fun capture_id ->
      List.find t.captures ~f:(fun capture ->
        Capture_id.equal (Capture_metadata.id capture) capture_id))
  ;;

  let selected_segment t = t.selected_segment
  let current_time_ms t = t.current_time_ms
  let draft_interval t = t.draft_interval
  let loaded_trace t = t.loaded_trace
  let labels t = List.rev t.labels
  let has_unsaved_changes t = t.has_unsaved_changes
end
