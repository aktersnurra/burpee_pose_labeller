open! Core

module Error : sig
  type t =
    | Invalid_interval of
        { start_ms : int
        ; end_ms : int
        }
    | Empty_label
    | No_interval_in_progress
    | Invalid_manifest of string
    | Invalid_labels of string
    | Manifest_file_error of
        { path : string
        ; message : string
        }
    | Label_file_error of
        { path : string
        ; message : string
        }
  [@@deriving compare, equal, sexp]
end

module Capture_id : sig
  type t [@@deriving compare, equal, sexp]

  val of_int : int -> t
  val to_int : t -> int
end

module Label_id : sig
  type t [@@deriving compare, equal, sexp]

  val of_int : int -> t
  val to_int : t -> int
end

module Segment : sig
  type t =
    | Warmup
    | Main
  [@@deriving compare, equal, sexp]
end

module Label_type : sig
  type t =
    | Phase
    | Rep
    | Quality
    | Tag
  [@@deriving compare, equal, sexp]
end

module Capture_metadata : sig
  type t [@@deriving compare, equal, sexp]

  val id : t -> Capture_id.t
  val recorded_at : t -> string option
  val session_name : t -> string option
  val has_segment : t -> Segment.t -> bool
  val sample_count : t -> Segment.t -> int option
  val model_name : t -> string option
  val model_version : t -> string option
  val labels_present : t -> bool
  val analysis_present : t -> bool
end

module Bundle_manifest : sig
  type t [@@deriving compare, equal, sexp]

  val parse_string : string -> (t, Error.t) result
  val load : bundle_dir:string -> (t, Error.t) result
  val captures : t -> Capture_metadata.t list
end

module Interval : sig
  type t [@@deriving compare, equal, sexp]

  val create : start_ms:int -> end_ms:int -> (t, Error.t) result
  val start_ms : t -> int
  val end_ms : t -> int
end

module Label : sig
  type t [@@deriving compare, equal, sexp]

  val create
    :  id:Label_id.t
    -> capture_id:Capture_id.t
    -> segment:Segment.t
    -> interval:Interval.t
    -> label_type:Label_type.t
    -> label:string
    -> (t, Error.t) result

  val id : t -> Label_id.t
  val capture_id : t -> Capture_id.t
  val segment : t -> Segment.t
  val interval : t -> Interval.t
  val label_type : t -> Label_type.t
  val label : t -> string
end

module Label_store : sig
  val parse_string : string -> (Label.t list, Error.t) result
  val to_string : Label.t list -> string
  val load_json_file : path:string -> (Label.t list, Error.t) result
  val save_json_file : path:string -> Label.t list -> (unit, Error.t) result
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

module Model : sig
  type t [@@deriving sexp]

  val empty : t
  val apply : t -> action -> (t, Error.t) result
  val apply_exn : t -> action -> t
  val selected_capture : t -> Capture_id.t option
  val selected_segment : t -> Segment.t
  val current_time_ms : t -> int
  val draft_interval : t -> Interval.t option
  val labels : t -> Label.t list
  val has_unsaved_changes : t -> bool
end
