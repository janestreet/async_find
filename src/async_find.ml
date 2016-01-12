open Core.Std
open Async.Std
module Stats = Unix.Stats

type file_info = string * Unix.Stats.t
type path = string list

let path_append path x = x :: path
let path_to_string ?base path =
  match (base, path) with
  | None,      [] -> "."
  | Some base, [] -> base
  | None,      _  ->         String.concat ~sep:"/" (List.rev path)
  | Some base, _  -> base ^/ String.concat ~sep:"/" (List.rev path)

module Options = struct
  type error_handler =
    | Ignore
    | Print
    | Raise
    | Handle_with of (string -> unit Deferred.t)

  type t = {
      min_depth: int;
      max_depth: int option;
      follow_links: bool;
      on_open_errors: error_handler;
      on_stat_errors: error_handler;
      filter: (file_info -> bool Deferred.t) option;
      skip_dir: (file_info -> bool Deferred.t) option;
      relative_paths : bool;
    }

  let default = {
      min_depth = 1;
      max_depth = None;
      follow_links = false;
      on_open_errors = Raise;
      on_stat_errors = Raise;
      filter = None;
      skip_dir = None;
      relative_paths = false;
    }

  let ignore_errors = { default with
      on_open_errors = Ignore;
      on_stat_errors = Ignore
    }
end
module O = Options

type t = {
  base: string;
  options: Options.t;
  already_seen: ((int * int), unit) Hashtbl.t;  (* device num * inode *)
  mutable to_visit: (path * int) list;  (* dir to traverse and the depth it is at *)
  mutable current_dir: path;
  mutable current_handle: [ `Just_created | `Starting | `Handle of Unix.dir_handle ];
  mutable depth: int;
  mutable closed: bool;
}

let full_path_name   t path =
  path_to_string ~base:t.base path
let output_path_name t path =
  path_to_string ?base:(if t.options.O.relative_paths then None else Some t.base) path

let open_next_dir t =
  let i = Ivar.create () in
  let rec loop t =
    match t.to_visit with
    | [] -> Ivar.fill i None
    | (dir_name, depth) :: rest ->
        upon (Monitor.try_with ~rest:`Raise (fun () ->
          t.to_visit <- rest;
          Unix.opendir (full_path_name t dir_name) >>| (fun handle ->
          t.current_handle <- `Handle handle;
          t.current_dir <- dir_name;
          t.depth <- depth;
          Some ())
        )) (function
        | Ok r -> Ivar.fill i r
        | Error e ->
          let e = Monitor.extract_exn e in
          match t.options.O.on_open_errors with
          | O.Ignore -> loop t
          | O.Raise -> raise e
          | O.Handle_with f ->
            upon (f (output_path_name t dir_name)) (fun () -> loop t)
          | O.Print ->
            Print.eprintf "unable to open %s - %s\n"
              (output_path_name t dir_name) (Exn.to_string e);
            loop t)
  in
  loop t;
  Ivar.read i
;;

let closedir t =
  match t.current_handle with
  | `Just_created | `Starting -> return ()
  | `Handle current_handle ->
    Deferred.ignore
      (Monitor.try_with ~rest:`Raise (fun () -> Unix.closedir current_handle)
         : (unit, exn) Result.t Deferred.t)

;;

let close t =
  if not t.closed then
    begin
      t.closed <- true;
      closedir t >>| fun () ->
      Hashtbl.clear t.already_seen;
      t.to_visit <- [];
    end
  else Deferred.unit
;;

(* return None if [fn] is a directory and has already been seen, otherwise Some info *)
let is_new t (_output_fn, _path, stats as info) =
  if stats.Stats.kind <> `Directory then
    return (Some info)
  else begin
    let uid = (stats.Stats.dev, stats.Stats.ino) in
    return
      (match Hashtbl.find t.already_seen uid with
      | Some () -> None
      | None ->
        Hashtbl.set t.already_seen ~key:uid ~data:();
        Some info)
  end
;;

let stat t path =
  let full_fn = full_path_name t path in
  let output_fn  = output_path_name t path in
  Monitor.try_with ~rest:`Raise (fun () ->
      let stat = if t.options.O.follow_links then Unix.stat else Unix.lstat in
      stat full_fn >>| (fun stat -> Some (output_fn, path, stat))
    ) >>= (function
    | Ok r -> return r
    | Error e ->
      let e = Monitor.extract_exn e in
      match t.options.O.on_stat_errors with
      | O.Ignore -> return None
      | O.Raise -> raise e
      | O.Handle_with f -> f output_fn >>| (fun () -> None)
      | O.Print ->
        Print.eprintf "unable to stat %s - %s\n" output_fn (Exn.to_string e);
        return None)
;;

let handle_dirs t (output_fn, path, stats) =
  let info = output_fn, stats in
  let visit () =
    t.to_visit <- (path, (t.depth + 1)) :: t.to_visit;
    return (Some info)
  in
  let maybe_visit () =
    match t.options.O.skip_dir with
    | None   -> visit ()
    | Some f ->
      f info
      >>= fun skip ->
      if skip then return None else visit ()
  in
  let maybe_return_info () =
    match t.options.O.skip_dir with
    | None   -> return (Some info)
    | Some f ->
      f info
      >>= fun skip ->
      if skip then return None else return (Some info)
  in
  if stats.Stats.kind = `Directory then begin
    match t.options.O.max_depth with
    | None           -> maybe_visit ()
    | Some max_depth ->
      if t.depth < max_depth then maybe_visit () else begin
        maybe_return_info ()
      end
  end else
    return (Some info)
;;

let filter t file =
  match file with
  | None      -> return None
  | Some file ->
      if t.depth < t.options.O.min_depth then
        return None
      else match t.options.O.filter with
      | None   -> return (Some file)
      | Some f -> f file >>| (fun keep -> if keep then Some file else None)
;;

exception Attempt_to_use_closed_find of [`Most_recent_dir of string] [@@deriving sexp] ;;

let ensure_not_closed t = if t.closed then
  raise (Attempt_to_use_closed_find
           (`Most_recent_dir (output_path_name t t.current_dir))) ;;

(* returns the next file from the conceptual stream and updates the state of t - this
   is the only way that t should ever be updated *)
let next t =
  ensure_not_closed t;
  let i = Ivar.create () in
  let handle_child path =
    (* each function in this bind returns None if the file should be skipped, and
       Some f i if it thinks it's ok to emit - possibly updating the state or
       transforming f along the way *)
    let (>>>=)
        : type v w. v option Deferred.t -> (v -> w option Deferred.t) -> w option Deferred.t
        = fun v f ->
          v
          >>= function
          | None   -> return None
          | Some v -> f v
    in
    stat t path
    >>>= is_new t
    >>>= handle_dirs t
    >>= filter t
  in
  let with_next_dir k =
    upon (open_next_dir t) (function
    | None -> upon (close t) (fun () -> Ivar.fill i None)
    | Some () -> k ())
  in
  let rec loop () =
    let handle_child_or_loop path =
      handle_child path
      >>> function
      | None -> loop ()
      | r    -> Ivar.fill i r
    in
    match t.current_handle with
    | `Just_created ->
      begin match t.options.O.max_depth with
      | Some d when d < 0 -> upon (close t) (fun () -> Ivar.fill i None)
      | None | Some _ ->
        t.current_handle <- `Starting;
        handle_child_or_loop t.current_dir
      end
    | `Starting ->
      with_next_dir loop
    | `Handle current_handle ->
      upon (Monitor.try_with ~rest:`Raise (fun () ->
        Unix.readdir current_handle)
      ) (function
      | Ok ("." | "..") -> loop ()
      | Ok basename -> handle_child_or_loop (path_append t.current_dir basename)
      | Error e ->
        upon (closedir t) (fun () ->
          match Monitor.extract_exn e with
          | End_of_file -> with_next_dir loop
          | e -> raise e))
  in
  loop ();
  Ivar.read i
;;

let create ?(options=Options.default) dir =
  {
    base = dir;
    options = options;
    already_seen = Hashtbl.Poly.create () ~size:11;
    to_visit = [];
    current_dir = [];
    current_handle = `Just_created;
    depth = 0;
    closed = false;
  }
;;

let fold t ~init ~f =
  Deferred.create (fun i ->
    let rec loop acc =
      upon (next t) (function
      | None -> Ivar.fill i acc
      | Some file -> upon (f acc file) loop)
    in
    loop init)
;;

let iter t ~f = fold t ~init:() ~f:(fun () file -> f file)

let to_list t =
  (fold t ~init:[] ~f:(fun acc file -> return (file :: acc))) >>| List.rev
;;

let find_all ?options dir =
  to_list (create ?options dir)
;;
