open! Core

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
