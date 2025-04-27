(** Async implementation of the find_files library * * Note: Unlike Unix [find], the
    functions in this module do not produce paths in * depth-first order. *)

open! Core
open Async

type t
type file_info = Filename.t * Unix.Stats.t

module Options : sig
  (** See {!Find_files.Options} for documentation. *)
  type error_handler =
    | Ignore
    | Print
    | Raise
    | Handle_with of (Filename.t -> exn -> unit Deferred.t)

  type t =
    { min_depth : int
    ; max_depth : int option
    ; follow_links : bool
    ; on_open_errors : error_handler
    ; on_stat_errors : error_handler
    ; filter : (file_info -> bool Deferred.t) option
    ; skip_dir : (file_info -> bool Deferred.t) option
    ; relative_paths : bool
    }

  val default : t
  val ignore_errors : t
end

(** [create ?options dir] create a Find.t based in dir *)
val create : ?options:Options.t -> string -> t

(** [with_close ?options dir ~f] create a Find.t based in dir, operate on it in the
    callback, and close it once the callback finishes. *)
val with_close : ?options:Options.t -> string -> f:(t -> 'a Deferred.t) -> 'a Deferred.t

(** [next t] return the next file from the collection of valid files in t or None if no
    more files remain *)
val next : t -> (string * Unix.Stats.t) option Deferred.t

(** [close t] drops all the resources associated with t. Attempting to use t again will
    raise an exception. Any Find.t will be automatically closed after the last file is
    read by any means. *)
val close : t -> unit Deferred.t

(** [iter t ~f] calls f on every file in t *)
val iter : t -> f:(string * Unix.Stats.t -> unit Deferred.t) -> unit Deferred.t

(** [fold t ~init ~f] folds f over the files in t *)
val fold
  :  t
  -> init:'a
  -> f:('a -> string * Unix.Stats.t -> 'a Deferred.t)
  -> 'a Deferred.t

(** [to_list t] returns all of the remaining files in t as a list in the order they would
    have been returned by subsequent calls to next *)
val to_list : t -> (string * Unix.Stats.t) list Deferred.t

(** [find_all ?options dir] short for to_list (create ?options dir) *)
val find_all : ?options:Options.t -> string -> (string * Unix.Stats.t) list Deferred.t
