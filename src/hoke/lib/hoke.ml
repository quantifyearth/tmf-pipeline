open Lwt.Infix

let engine url : Current_rpc.Engine.t Lwt.t =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let sr = Capnp_rpc_unix.Vat.import_exn vat url in
  Capnp_rpc_lwt.Sturdy_ref.connect_exn sr

(* Terminal UI *)
open Nottui
module W = Nottui_widgets

type job = {
  job : Current_rpc.Job.t;
  status : Current_rpc.Job.status;
  selected : bool;
  expanded : bool;
  log : string option;
}

module JobTable = struct
  type t = {
    table : job Lwd_table.t;
    mutable selected : job Lwd_table.row option;
  }

  let selected t =
    match t.selected with None -> None | Some row -> Lwd_table.get row

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

  let set_log job_row =
    match Lwd_table.get job_row with
    | None -> ()
    | Some job ->
        let buf = Buffer.create 128 in
        let rec aux start =
          Current_rpc.Job.log ~start job.job >>= function
          | Error _ as e -> Lwt.return e
          | Ok (data, next) ->
              if data = "" then Lwt_result.return ()
              else (
                Buffer.add_string buf data;
                aux next)
        in
        Lwt.async (fun () ->
            aux 0L >|= function
            | Ok () ->
                Lwd_table.set job_row
                  { job with log = Some (Buffer.contents buf); expanded = true }
            | _ ->
                Lwd_table.set job_row
                  { job with log = Some "Failed to get log"; expanded = true })

  let toggle_selected t =
    match t.selected with
    | None -> ()
    | Some r -> (
        match Lwd_table.get r with
        | None -> ()
        | Some v ->
            if (not v.expanded) && v.log = None then set_log r
            else Lwd_table.set r { v with expanded = not v.expanded })
end

let job_ui (job : job) =
  let attr =
    Notty.A.(
      fg green ++ if job.selected then Notty.A.(bg white) else Notty.A.empty)
  in
  let arrow = if job.expanded then "▼ " else "▶ " in
  let base =
    if job.expanded then
      W.fmt ~attr "%s%s" arrow job.status.id
      :: (String.split_on_char '\n' job.status.description
         |> List.map (W.fmt ~attr:Notty.A.(st italic) "%s"))
    else [ W.fmt ~attr "%s%s" arrow job.status.id ]
  in
  Ui.vcat base

let job_view (job : job) =
  let attr = Notty.A.(fg green ++ st italic) in
  let base =
    [
      W.fmt ~attr "%s" job.status.id;
      W.fmt ~attr:Notty.A.(st italic) "%s" job.status.description;
    ]
    @ match job.log with Some log -> [ W.fmt "%s" log ] | None -> []
  in
  W.scroll_area @@ Lwd.return @@ Ui.vcat base

type screen = [ `Main | `Job of job ]

let ui engine =
  let quit, resolve_quit = Lwt.wait () in
  let screen : screen Lwd.var = Lwd.var `Main in
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
              (fun j ->
                Current_rpc.Job.status j >|= Result.get_ok >|= fun v -> (v, j))
              js
            >>= fun js ->
            let js =
              List.map
                (fun (status, job) ->
                  {
                    job;
                    status;
                    selected = false;
                    expanded = false;
                    log = None;
                  })
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
        | `ASCII 'e', [] -> (
            match JobTable.selected jobs with
            | Some job ->
                Lwd.set screen (`Job job);
                `Handled
            | None -> `Handled)
        | `Enter, [] ->
            JobTable.toggle_selected jobs;
            `Handled
        | (`Escape | `ASCII 'q'), [] ->
            Lwt.wakeup_later resolve_quit ();
            `Handled
        | _ -> `Unhandled)
      ui
  in
  let ui =
    let view =
      Lwd.bind (Lwd.get screen) ~f:(function
        | `Main -> lst
        | `Job job -> job_view job)
    in
    W.window_manager_view (W.window_manager view)
  in
  Nottui_lwt.run ~quit (Lwd.map ~f:keys ui)
