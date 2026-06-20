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

  let captures t = t.captures
end

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

type action =
  | Select_capture of Capture_id.t
  | Select_segment of Segment.t
  | Seek of int
  | Start_interval of int
  | End_interval of int
  | Add_label of Label.t
  | Edit_label of Label.t
  | Delete_label of Label_id.t
  | Mark_saved
[@@deriving sexp]

module Model = struct
  type t =
    { selected_capture : Capture_id.t option
    ; selected_segment : Segment.t
    ; current_time_ms : int
    ; interval_start_ms : int option
    ; draft_interval : Interval.t option
    ; labels : Label.t list
    ; has_unsaved_changes : bool
    }
  [@@deriving sexp]

  let empty =
    { selected_capture = None
    ; selected_segment = Main
    ; current_time_ms = 0
    ; interval_start_ms = None
    ; draft_interval = None
    ; labels = []
    ; has_unsaved_changes = false
    }
  ;;

  let apply t = function
    | Select_capture selected_capture -> Ok { t with selected_capture = Some selected_capture }
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

  let selected_capture t = t.selected_capture
  let selected_segment t = t.selected_segment
  let current_time_ms t = t.current_time_ms
  let draft_interval t = t.draft_interval
  let labels t = List.rev t.labels
  let has_unsaved_changes t = t.has_unsaved_changes
end
