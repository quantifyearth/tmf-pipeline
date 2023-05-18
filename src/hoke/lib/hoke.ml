open Lwt.Infix

let engine url : Current_rpc.Engine.t Lwt.t =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let sr = Capnp_rpc_unix.Vat.import_exn vat url in
  Capnp_rpc_lwt.Sturdy_ref.connect_exn sr

(* Terminal UI *)
open Nottui
module W = Nottui_widgets

type job = { status : Current_rpc.Job.status; selected : bool; expanded : bool }

module JobTable = struct
  type t = {
    table : job Lwd_table.t;
    mutable selected : job Lwd_table.row option;
  }

  let create () = { table = Lwd_table.make (); selected = None }

  let update (t : t) lst =
    let rec need_appending ((acc, selected) : job list * bool)
        (row : job Lwd_table.row option) =
      match row with
      | None -> (acc, selected)
      | Some row ->
          let job = Lwd_table.get row |> Option.get in
          let selected = if selected then selected else job.selected in
          need_appending
            (List.filter (fun j -> j.status.id <> job.status.id) acc, selected)
            (Lwd_table.next row)
    in
    let need, selected =
      Lwd_table.first t.table |> need_appending (lst, false)
    in
    List.iter (Lwd_table.append' t.table) need;
    if not selected then
      match Lwd_table.first t.table with
      | None -> ()
      | Some row -> (
          match Lwd_table.get row with
          | None -> ()
          | Some j ->
              Lwd_table.set row { j with selected = true };
              t.selected <- Some row)

  let select t dir =
    let to_select row = function
      | `Up ->
          let r = Lwd_table.prev row in
          if Option.is_none r then Lwd_table.last t.table else r
      | `Down ->
          let r = Lwd_table.next row in
          if Option.is_none r then Lwd_table.first t.table else r
    in
    match t.selected with
    | None -> ()
    | Some row -> (
        match to_select row dir with
        | None -> ()
        | Some row' ->
            t.selected <- Some row';
            let job = Lwd_table.get row |> Option.get in
            let job' = Lwd_table.get row' |> Option.get in
            Lwd_table.set row { job with selected = false };
            Lwd_table.set row' { job' with selected = true })

  let toggle_selected t =
    match t.selected with
    | None -> ()
    | Some r -> (
        match Lwd_table.get r with
        | None -> ()
        | Some v -> Lwd_table.set r { v with expanded = not v.expanded })
end

let job_ui (job : job) =
  let attr =
    Notty.A.(
      fg green ++ if job.selected then Notty.A.(bg white) else Notty.A.empty)
  in
  let base =
    if job.expanded then
      [ W.fmt ~attr:Notty.A.(st italic) "%s" job.status.description ]
    else []
  in
  let arrow = if job.expanded then "▼ " else "▶ " in
  Ui.vcat (W.fmt ~attr "%s%s" arrow job.status.id :: base)

let ui engine =
  let quit, resolve_quit = Lwt.wait () in
  let jobs : JobTable.t = JobTable.create () in
  let lst =
    Lwd_table.map_reduce
      (fun _ job -> job_ui job)
      (Ui.empty, Ui.join_y) jobs.table
  in
  Lwt.async (fun () ->
      let rec loop () =
        Current_rpc.Engine.active_jobs engine >>= function
        | Ok h ->
            let js = List.map (Current_rpc.Engine.job engine) h in
            Lwt_list.map_p
              (fun j -> Current_rpc.Job.status j >|= Result.get_ok)
              js
            >>= fun js ->
            let js =
              List.map
                (fun status -> { status; selected = false; expanded = false })
                js
            in
            JobTable.update jobs js;
            Lwt_unix.sleep 1. >>= loop
        | _ -> Lwt_unix.sleep 1. >>= loop
      in
      loop ());
  let keys ui =
    Ui.keyboard_area
      (function
        | `Arrow `Up, [] ->
            JobTable.select jobs `Up;
            `Handled
        | `Arrow `Down, [] ->
            JobTable.select jobs `Down;
            `Handled
        | `Enter, [] ->
            JobTable.toggle_selected jobs;
            `Handled
        | (`Escape | `ASCII 'q'), [] ->
            Lwt.wakeup_later resolve_quit ();
            `Handled
        | _ -> `Unhandled)
      ui
  in
  let ui = W.window_manager_view (W.window_manager lst) in
  Nottui_lwt.run ~quit (Lwd.map ~f:keys ui)
