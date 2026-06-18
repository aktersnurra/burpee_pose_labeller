open! Core

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
