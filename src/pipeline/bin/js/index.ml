open Brr
open Brr_io
open Xterm

(* A console for logging into a container in the pipeline.
   The Websocket proxies onto a Capnp which is talking to the
   container. *)
let command = ref @@ Jstr.empty

let run_command ws term =
  let l = Jstr.length !command in
  if l > 0 then begin
    Websocket.send_string ws !command;
    for _ = 0 to l - 2 do
      Terminal.write' term "\b \b"
    done;
  end

let setup term ws =
  let on_data jstr =
    match Jstr.to_string jstr with
    | "\r" (* Enter *) ->
      command := Jstr.(!command + jstr);
      run_command ws term;
      command := Jstr.empty
    | "\u{007F}" (* Backspace *) ->
      (* Allows people to delete terminal prompt :/ *)
      Terminal.write' term "\b \b";
      let cmd_length = Jstr.length !command in
      if cmd_length > 0 then begin
        command := Jstr.sub ~start:0 ~len:(cmd_length - 1) !command
      end
    | _ ->
      command := Jstr.(!command + jstr);
      Terminal.write term jstr
  in
  let _ : Ev.listener = Ev.listen Message.Ev.message (fun msg ->
    let msg = Ev.as_type msg in
    let data : Jstr.t = Message.Ev.data msg in
    Terminal.write term data
  ) (Websocket.as_target ws)
  in
  Terminal.on_data term on_data

let () =
  match Document.find_el_by_id G.document (Jstr.v "terminal") with
  | None -> failwith "No terminal element"
  | Some el ->
    let opts = Terminal.opts ~cursor_blink:true () in
    let terminal = Xterm.Terminal.v ~opts () in
    Terminal.open_ terminal el;
    let uri = Window.location G.window in
    let uri = Uri.with_uri ~scheme:(Jstr.v "ws") uri |> Result.get_ok |> Uri.to_jstr in
    let websocket = Websocket.create (Jstr.(uri + v "/connect")) in
    setup terminal websocket